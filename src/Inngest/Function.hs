{-# LANGUAGE OverloadedStrings #-}

-- | Durable-function definitions and their sync-time 'FunctionConfig'
-- serialization (spec §6). A function carries its full config surface plus a
-- handler; @onFailure@ is emitted as a second, @-failure@-suffixed config
-- triggered by @inngest/function.failed@.
module Inngest.Function
  ( -- * Functions
    Function(..)
  , FnOpts(..)
  , defaultFnOpts
  , createFunction
  , withOnFailure
  , Handler
    -- * Config surface
  , Concurrency(..)
  , RateLimit(..)
  , Debounce(..)
  , Throttle(..)
  , Batch(..)
  , Cancel(..)
  , Timeouts(..)
  , Priority(..)
  , Singleton(..)
  , SingletonMode(..)
    -- * Sync serialization
  , functionConfigs
  , functionUrl
  ) where

import Data.Aeson
import Data.Aeson.Types (Pair)
import Data.Text (Text)
import qualified Data.Text as T

import Inngest.Types
import Inngest.Step (Ctx, InngestT)

--------------------------------------------------------------------------------
-- Config sub-records
--------------------------------------------------------------------------------

data Concurrency = Concurrency
  { conLimit :: Int
  , conKey   :: Maybe Text
  , conScope :: Maybe Text   -- ^ "account" | "env" | "fn"
  } deriving (Eq, Show)

data RateLimit = RateLimit
  { rlLimit  :: Int
  , rlPeriod :: Duration
  , rlKey    :: Maybe Text
  } deriving (Eq, Show)

data Debounce = Debounce
  { dbPeriod  :: Duration
  , dbKey     :: Maybe Text
  , dbTimeout :: Maybe Duration
  } deriving (Eq, Show)

data Throttle = Throttle
  { thLimit  :: Int
  , thPeriod :: Duration
  , thKey    :: Maybe Text
  , thBurst  :: Maybe Int
  } deriving (Eq, Show)

data Batch = Batch
  { bMaxSize :: Int
  , bTimeout :: Maybe Duration
  , bKey     :: Maybe Text
  , bIf      :: Maybe Text
  } deriving (Eq, Show)

data Cancel = Cancel
  { cEvent   :: Text
  , cIf      :: Maybe Text
  , cTimeout :: Maybe Duration
  } deriving (Eq, Show)

data Timeouts = Timeouts
  { toStart  :: Maybe Duration
  , toFinish :: Maybe Duration
  } deriving (Eq, Show)

newtype Priority = Priority { prRun :: Text } deriving (Eq, Show)

data SingletonMode = SingletonSkip | SingletonCancel deriving (Eq, Show)

data Singleton = Singleton
  { sgKey  :: Maybe Text
  , sgMode :: SingletonMode
  } deriving (Eq, Show)

-- Total duration encoder: valid Inngest duration string, else a millisecond
-- fallback (only reachable for sub-second config durations, which are invalid).
durText :: Duration -> Text
durText d = either (const (T.pack (show (durMillis d)) <> "ms")) id (toDurationStr d)

obj :: [Maybe Pair] -> Value
obj = object . concatMap (maybe [] pure)

infixr 8 .=?
(.=?) :: ToJSON v => Key -> Maybe v -> Maybe Pair
_ .=? Nothing  = Nothing
k .=? (Just v) = Just (k .= v)

req :: ToJSON v => Key -> v -> Maybe Pair
req k v = Just (k .= v)

instance ToJSON Concurrency where
  toJSON c = obj [ req "limit" (conLimit c), "key" .=? conKey c, "scope" .=? conScope c ]

instance ToJSON RateLimit where
  toJSON r = obj [ req "limit" (rlLimit r), req "period" (durText (rlPeriod r)), "key" .=? rlKey r ]

instance ToJSON Debounce where
  toJSON d = obj [ req "period" (durText (dbPeriod d)), "key" .=? dbKey d
                 , "timeout" .=? fmap durText (dbTimeout d) ]

instance ToJSON Throttle where
  toJSON t = obj [ req "limit" (thLimit t), req "period" (durText (thPeriod t))
                 , "key" .=? thKey t, "burst" .=? thBurst t ]

instance ToJSON Batch where
  toJSON b = obj [ req "maxSize" (bMaxSize b), "timeout" .=? fmap durText (bTimeout b)
                 , "key" .=? bKey b, "if" .=? bIf b ]

instance ToJSON Cancel where
  toJSON c = obj [ req "event" (cEvent c), "if" .=? cIf c
                 , "timeout" .=? fmap durText (cTimeout c) ]

instance ToJSON Timeouts where
  toJSON t = obj [ "start" .=? fmap durText (toStart t), "finish" .=? fmap durText (toFinish t) ]

instance ToJSON Priority where
  toJSON p = object [ "run" .= prRun p ]

instance ToJSON Singleton where
  toJSON s = obj [ "key" .=? sgKey s
                 , req "mode" (case sgMode s of SingletonSkip -> "skip" :: Text; SingletonCancel -> "cancel") ]

--------------------------------------------------------------------------------
-- FnOpts / Function
--------------------------------------------------------------------------------

-- | A handler maps the run context and triggering event to a durable
-- computation whose (JSON-encoded) result is the function output.
type Handler m = Ctx -> Event -> InngestT m Value

data FnOpts = FnOpts
  { foId          :: Text          -- ^ local id, unique within the app
  , foName        :: Maybe Text
  , foRetries     :: Maybe Int
  , foConcurrency :: [Concurrency]
  , foRateLimit   :: Maybe RateLimit
  , foDebounce    :: Maybe Debounce
  , foThrottle    :: Maybe Throttle
  , foBatchEvents :: Maybe Batch
  , foCancel      :: [Cancel]
  , foTimeouts    :: Maybe Timeouts
  , foPriority    :: Maybe Priority
  , foIdempotency :: Maybe Text
  , foSingleton   :: Maybe Singleton
  }

-- | Minimal options for a function with the given local id.
defaultFnOpts :: Text -> FnOpts
defaultFnOpts i = FnOpts i Nothing Nothing [] Nothing Nothing Nothing Nothing [] Nothing Nothing Nothing Nothing

data Function m = Function
  { fnOpts      :: FnOpts
  , fnTriggers  :: [Trigger]
  , fnHandler   :: Handler m
  , fnOnFailure :: Maybe (Handler m)
  }

-- | Build a function. The handler's result is JSON-encoded as the function
-- output.
createFunction
  :: (ToJSON a, Monad m)
  => FnOpts
  -> [Trigger]
  -> (Ctx -> Event -> InngestT m a)
  -> Function m
createFunction opts triggers h =
  Function opts triggers (\ctx ev -> toJSON <$> h ctx ev) Nothing

-- | Attach an on-failure handler (fires on terminal failure via
-- @inngest/function.failed@).
withOnFailure :: (ToJSON a, Monad m) => (Ctx -> Event -> InngestT m a) -> Function m -> Function m
withOnFailure h fn = fn { fnOnFailure = Just (\ctx ev -> toJSON <$> h ctx ev) }

--------------------------------------------------------------------------------
-- Sync serialization
--------------------------------------------------------------------------------

-- | The serve URL that targets a given function id for the root step.
functionUrl :: Text -> Text -> Text
functionUrl appUrl fqId = appUrl <> "?fnId=" <> fqId <> "&stepId=step"

-- | The fully-qualified id for a function within an app.
fqIdOf :: Text -> FnOpts -> Text
fqIdOf appId opts = appId <> "-" <> foId opts

stepEntry :: Text -> Maybe Int -> Value
stepEntry url retries = obj
  [ req "id" ("step" :: Text)
  , req "name" ("step" :: Text)
  , "retries" .=? fmap (\n -> object ["attempts" .= n]) retries
  , req "runtime" (object [ "type" .= ("http" :: Text), "url" .= url ])
  ]

-- | Serialize a function to its sync-time config(s): the main config, plus a
-- second @-failure@ config when an on-failure handler is present.
functionConfigs :: Text -> Text -> Function m -> [Value]
functionConfigs appId appUrl fn = mainCfg : failCfg
  where
    opts = fnOpts fn
    fqId = fqIdOf appId opts
    name = maybe (foId opts) id (foName opts)

    mainCfg = obj $
      [ req "id" fqId
      , req "name" name
      , req "triggers" (fnTriggers fn)
      , req "steps" (object [ "step" .= stepEntry (functionUrl appUrl fqId) (foRetries opts) ])
      , listOpt "concurrency" (foConcurrency opts)
      , "rateLimit"   .=? foRateLimit opts
      , "debounce"    .=? foDebounce opts
      , "throttle"    .=? foThrottle opts
      , "batchEvents" .=? foBatchEvents opts
      , listOpt "cancel" (foCancel opts)
      , "timeouts"    .=? foTimeouts opts
      , "priority"    .=? foPriority opts
      , "idempotency" .=? foIdempotency opts
      , "singleton"   .=? foSingleton opts
      ]

    failCfg = case fnOnFailure fn of
      Nothing -> []
      Just _  -> [ obj
        [ req "id" (fqId <> "-failure")
        , req "name" (name <> " (failure)")
        , req "triggers"
            [ TriggerEvent "inngest/function.failed"
                (Just ("event.data.function_id == '" <> fqId <> "'")) ]
        , req "steps" (object [ "step" .= stepEntry (functionUrl appUrl (fqId <> "-failure")) (Just 0) ])
        ] ]

    listOpt _ [] = Nothing
    listOpt k xs = Just (k .= xs)
