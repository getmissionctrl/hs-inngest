{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The durable step DSL and its stateless-replay engine.
--
-- Inngest re-invokes a function repeatedly, feeding back completed step results;
-- we re-run from the top, return memoized results for finished steps, and
-- interrupt (via 'ExceptT') at the first new step. This is a faithful port of
-- @inngest-py@'s interrupt-as-exception model.
module Inngest.Step
  ( -- * The monad
    InngestT(..)
  , MonadInngest
  , Interrupt(..)
  , runInngestT
    -- * Context and state
  , Ctx(..)
  , ExecState(..)
  , newExecState
  , mkCtx
    -- * Step combinators
  , stepRun
  , sleep
  , sleepUntil
  , waitForEvent
  , WaitOpts(..)
    -- * Errors
  , StepFailure(..)
    -- * Internals (exposed for testing)
  , hashStepId
  , nextHashedId
  , Memo(..)
  , parseMemo
  ) where

import Control.Monad.Reader (ReaderT(..), runReaderT, ask)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (fromMaybe)
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import Data.ByteString (ByteString)
import Crypto.Hash (hashWith, SHA1(..))
import Data.ByteArray.Encoding (convertToBase, Base(Base16))
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import UnliftIO (MonadUnliftIO)
import UnliftIO.Exception (try, throwIO, Exception, SomeException, displayException)

import Inngest.Types

--------------------------------------------------------------------------------
-- Monad
--------------------------------------------------------------------------------

-- | The base-monad constraint. 'MonadUnliftIO' gives us IO for the memo IORefs,
-- exception catching to map step failures to opcodes, and a clean boundary into
-- a serve layer's app monad.
type MonadInngest m = MonadUnliftIO m

-- | Control-flow signal carried in 'ExceptT' — NOT a runtime exception.
data Interrupt
  = Interrupt [OutgoingOp]  -- ^ opcode(s) to return to the server (206)
  | SkipStep                -- ^ a non-targeted step under stepId targeting
  deriving (Show, Eq)

newtype InngestT m a =
  InngestT { unInngestT :: ReaderT ExecState (ExceptT Interrupt m) a }
  deriving (Functor, Applicative, Monad, MonadIO)

runInngestT :: ExecState -> InngestT m a -> m (Either Interrupt a)
runInngestT st = runExceptT . flip runReaderT st . unInngestT

--------------------------------------------------------------------------------
-- Context / state
--------------------------------------------------------------------------------

-- | What a user handler sees for the current run.
data Ctx = Ctx
  { ctxRunId       :: Text
  , ctxAttempt     :: Int
  , ctxMaxAttempts :: Maybe Int
  , ctxEvent       :: Event
  , ctxEvents      :: [Event]
  } deriving (Show)

data ExecState = ExecState
  { esMemos            :: IORef (Map HashedStepId Value)
  , esCounter          :: IORef (Map Text Int)
  , esCtx              :: Ctx
  , esParallel         :: Bool
  , esTarget           :: Maybe HashedStepId
  , esDisableImmediate :: Bool
  }

-- | Build the user-facing 'Ctx' from an incoming request.
mkCtx :: ServerRequest -> Ctx
mkCtx sr = Ctx
  { ctxRunId       = scRunId (srCtx sr)
  , ctxAttempt     = scAttempt (srCtx sr)
  , ctxMaxAttempts = scMaxAttempts (srCtx sr)
  , ctxEvent       = srEvent sr
  , ctxEvents      = srEvents sr
  }

-- | Fresh execution state seeded from the request's memoized steps. @target@ is
-- the hashed step id to run under stepId targeting (Nothing = no targeting /
-- root "step").
newExecState :: ServerRequest -> Maybe HashedStepId -> IO ExecState
newExecState sr target = do
  memos   <- newIORef (srSteps sr)
  counter <- newIORef Map.empty
  pure ExecState
    { esMemos            = memos
    , esCounter          = counter
    , esCtx              = mkCtx sr
    , esParallel         = False
    , esTarget           = target
    , esDisableImmediate = scDisableImmediateExecution (srCtx sr)
    }

--------------------------------------------------------------------------------
-- Step-id hashing + dedup counter
--------------------------------------------------------------------------------

-- | SHA1 (hex) of the UTF-8 bytes of a (pre-hashed) step id.
hashStepId :: Text -> HashedStepId
hashStepId = decodeUtf8 . sha1hex . encodeUtf8
  where
    sha1hex :: ByteString -> ByteString
    sha1hex = convertToBase Base16 . hashWith SHA1

-- | Increment the per-run reuse counter for a user step id and return its hashed
-- id plus the reuse index (Nothing on first use, Just n for the n-th reuse). The
-- n-th reuse (n>1) hashes @"<id>:<n-1>"@.
nextHashedId :: ExecState -> Text -> IO (HashedStepId, Maybe Int)
nextHashedId st sid = do
  m <- readIORef (esCounter st)
  let n = maybe 1 (+ 1) (Map.lookup sid m)
  writeIORef (esCounter st) (Map.insert sid n m)
  let preHashed = if n > 1 then sid <> ":" <> T.pack (show (n - 1)) else sid
      idx       = if n > 1 then Just (n - 1) else Nothing
  pure (hashStepId preHashed, idx)

--------------------------------------------------------------------------------
-- Memoization
--------------------------------------------------------------------------------

-- | A consumed memo: a recorded success payload or a recorded error.
data Memo = MemoData Value | MemoError ErrorData
  deriving (Eq, Show)

-- | Interpret a raw incoming step result. A wrapper object whose only keys are
-- @data@/@error@ is an @Output@; anything else (e.g. a bare fulfilling event
-- object, which has a @name@ key) is taken as the data verbatim.
parseMemo :: Value -> Memo
parseMemo v = case v of
  Object o
    | all (\k -> K.toText k `elem` ["data", "error"]) (KM.keys o) ->
        case KM.lookup "error" o of
          Just err | err /= Null -> MemoError (parseErr err)
          _                      -> MemoData (fromMaybe Null (KM.lookup "data" o))
  _ -> MemoData v
  where
    parseErr (Object eo) =
      ErrorData (txt "code" eo) (txt "message" eo) (txtDef "name" eo "Error") (mtxt "stack" eo)
    parseErr _ = ErrorData "unknown" "" "Error" Nothing
    txt k o = case KM.lookup k o of { Just (String s) -> s; _ -> "" }
    txtDef k o d = case KM.lookup k o of { Just (String s) -> s; _ -> d }
    mtxt k o = case KM.lookup k o of { Just (String s) -> Just s; _ -> Nothing }

-- Pop a memo by hashed id, removing it from the map.
popMemo :: ExecState -> HashedStepId -> IO (Maybe Memo)
popMemo st h = do
  m <- readIORef (esMemos st)
  case Map.lookup h m of
    Nothing -> pure Nothing
    Just v  -> do
      writeIORef (esMemos st) (Map.delete h m)
      pure (Just (parseMemo v))

--------------------------------------------------------------------------------
-- Errors
--------------------------------------------------------------------------------

-- | A memoized step error, rethrown into user code on replay (mirrors
-- @inngest-py@ raising @StepError@ at the call site).
newtype StepFailure = StepFailure ErrorData
  deriving (Show)
instance Exception StepFailure

--------------------------------------------------------------------------------
-- Interrupt helpers
--------------------------------------------------------------------------------

interrupt :: Monad m => [OutgoingOp] -> InngestT m a
interrupt ops = InngestT (lift (throwE (Interrupt ops)))

skip :: Monad m => InngestT m a
skip = InngestT (lift (throwE SkipStep))

--------------------------------------------------------------------------------
-- Step combinators
--------------------------------------------------------------------------------

-- | Run a durable step. On replay the memoized output is decoded and returned; a
-- memoized error is rethrown. On a new step the body executes and an interrupt
-- carries the result back to the server.
stepRun :: (ToJSON a, FromJSON a, MonadInngest m) => Text -> m a -> InngestT m a
stepRun sid body = do
  st <- InngestT ask
  (hashed, idx) <- liftIO (nextHashedId st sid)
  memo <- liftIO (popMemo st hashed)
  case memo of
    Just (MemoData d) -> case fromJSON d of
      Success a -> pure a
      Error e   -> liftIO (throwIO (StepFailure
                     (ErrorData "unknown" (T.pack ("failed to decode step output: " <> e)) "DecodeError" Nothing)))
    Just (MemoError ed) -> liftIO (throwIO (StepFailure ed))
    Nothing -> case esTarget st of
      Just t | hashed /= t -> skip
      _ | targetingOff st && esParallel st         -> interrupt [planned hashed sid idx]
        | targetingOff st && esDisableImmediate st  -> interrupt [planned hashed sid idx]
        | otherwise -> do
            res <- runBody body
            case res of
              Right out -> interrupt [ OutgoingOp (userland (stepInfo hashed OpStepRun sid) sid idx)
                                                  (Just (toJSON out)) Nothing ]
              Left err  -> interrupt [ stepErrorOp st hashed sid idx err ]
  where
    targetingOff s = case esTarget s of { Nothing -> True; Just _ -> False }

-- | Run a user step body in @m@, catching synchronous exceptions.
runBody :: MonadInngest m => m a -> InngestT m (Either SomeException a)
runBody body = InngestT (lift (lift (try body)))

planned :: HashedStepId -> Text -> Maybe Int -> OutgoingOp
planned hashed sid idx =
  OutgoingOp (userland (stepInfo hashed OpStepPlanned sid) sid idx) Nothing Nothing

userland :: StepInfo -> Text -> Maybe Int -> StepInfo
userland si sid idx = si { siUserland = Just (Userland sid idx) }

stepErrorOp :: ExecState -> HashedStepId -> Text -> Maybe Int -> SomeException -> OutgoingOp
stepErrorOp st hashed sid idx err =
  OutgoingOp (userland (stepInfo hashed op sid) sid idx) Nothing (Just ed)
  where
    terminal = case ctxMaxAttempts (esCtx st) of
      Just ma -> ctxAttempt (esCtx st) + 1 >= ma
      Nothing -> False
    op = if terminal then OpStepFailed else OpStepError
    ed = ErrorData "unknown" (T.pack (displayException err)) "Error" Nothing

-- | Sleep for a duration (measured from now).
sleep :: MonadInngest m => Text -> Duration -> InngestT m ()
sleep sid dur = do
  now <- liftIO getCurrentTime
  let until_ = addUTCTime (fromIntegral (durMillis dur) / 1000) now
  sleepUntil sid until_

-- | Sleep until an absolute time.
sleepUntil :: MonadInngest m => Text -> UTCTime -> InngestT m ()
sleepUntil sid until_ = do
  st <- InngestT ask
  (hashed, idx) <- liftIO (nextHashedId st sid)
  memo <- liftIO (popMemo st hashed)
  case memo of
    Just _  -> pure ()   -- already slept
    Nothing -> case esTarget st of
      Just t | hashed /= t -> skip
      _ -> interrupt
             [ OutgoingOp (userland (stepInfo hashed OpSleep sid)
                                    { siName = Just (toIsoUtc until_) } sid idx)
                          Nothing Nothing ]

-- | Options for 'waitForEvent'.
data WaitOpts = WaitOpts
  { woEvent   :: Text          -- ^ the event name to await
  , woTimeout :: Duration      -- ^ how long to wait
  , woIf      :: Maybe Text    -- ^ optional CEL match expression
  }

-- | Wait for a matching event. Returns @Just event@ when one arrives, or
-- @Nothing@ on timeout.
waitForEvent :: MonadInngest m => Text -> WaitOpts -> InngestT m (Maybe Event)
waitForEvent sid opts = do
  st <- InngestT ask
  (hashed, idx) <- liftIO (nextHashedId st sid)
  memo <- liftIO (popMemo st hashed)
  case memo of
    Just (MemoData Null) -> pure Nothing               -- timed out
    Just (MemoData v)    -> case fromJSON v of
      Success ev -> pure (Just ev)
      Error _    -> pure Nothing
    Just (MemoError ed)  -> liftIO (throwIO (StepFailure ed))
    Nothing -> case esTarget st of
      Just t | hashed /= t -> skip
      _ -> case toDurationStr (woTimeout opts) of
        Left e   -> liftIO (throwIO (StepFailure (ErrorData "unknown" (T.pack e) "ConfigError" Nothing)))
        Right ts -> interrupt
          [ OutgoingOp (userland (stepInfo hashed OpWaitForEvent sid)
                          { siName = Just (woEvent opts)
                          , siOpts = Just (object (("timeout" .= ts)
                                                   : maybe [] (\e -> ["if" .= e]) (woIf opts))) }
                          sid idx)
                       Nothing Nothing ]

