{-# LANGUAGE OverloadedStrings #-}

module Inngest.Types
  ( -- * Events
    Event(..)
  , mkEvent
  , EventId
    -- * Opcodes
  , Opcode(..)
  , opcodeText
  , HashedStepId
  , Userland(..)
  , StepInfo(..)
  , ErrorData(..)
  , OutgoingOp(..)
  , stepInfo
  , encodeOp
    -- * Duration
  , Duration(..)
  , durMillis
  , seconds, minutes, hours, days, weeks
  , toDurationStr
  , toIsoUtc
    -- * Triggers
  , Trigger(..)
    -- * Incoming server request
  , ServerCtx(..)
  , ServerRequest(..)
  ) where

import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson.KeyMap as KM
import Data.Map.Strict (Map)
import Data.Time (formatTime, defaultTimeLocale)
import Data.Time.Clock (UTCTime(..), diffTimeToPicoseconds)

--------------------------------------------------------------------------------
-- Events
--------------------------------------------------------------------------------

type EventId = Text

data Event = Event
  { eventName :: Text
  , eventData :: Value
  , eventId   :: Maybe Text
  , eventTs   :: Maybe Int    -- epoch milliseconds
  , eventVer  :: Maybe Text
  } deriving (Eq, Show)

-- | An event with only the required fields set.
mkEvent :: Text -> Value -> Event
mkEvent n d = Event n d Nothing Nothing Nothing

instance ToJSON Event where
  toJSON e = Object $ KM.fromList $
       [ ("name", toJSON (eventName e))
       , ("data", eventData e) ]
    ++ opt "id" (eventId e)
    ++ opt "ts" (eventTs e)
    ++ opt "v"  (eventVer e)
    where
      opt _ Nothing  = []
      opt k (Just v) = [(k, toJSON v)]

instance FromJSON Event where
  parseJSON = withObject "Event" $ \o -> Event
    <$> o .:  "name"
    <*> o .:? "data" .!= Null
    <*> o .:? "id"
    <*> o .:? "ts"
    <*> o .:? "v"

--------------------------------------------------------------------------------
-- Opcodes
--------------------------------------------------------------------------------

type HashedStepId = Text

-- | Opcode discriminator emitted in the 206 response array. String values match
-- the Inngest @Opcode@ enum exactly.
data Opcode
  = OpStepRun
  | OpSleep
  | OpWaitForEvent
  | OpInvokeFunction
  | OpStepPlanned
  | OpStepError
  | OpStepFailed
  | OpAIGateway
  deriving (Eq, Show)

opcodeText :: Opcode -> Text
opcodeText o = case o of
  OpStepRun        -> "StepRun"
  OpSleep          -> "Sleep"
  OpWaitForEvent   -> "WaitForEvent"
  OpInvokeFunction -> "InvokeFunction"
  OpStepPlanned    -> "StepPlanned"
  OpStepError      -> "StepError"
  OpStepFailed     -> "StepFailed"
  OpAIGateway      -> "AIGateway"

instance ToJSON Opcode where
  toJSON = String . opcodeText

-- | The @userland@ annotation attached to a step: the user-facing id plus the
-- reuse index (Nothing on first use of an id, Just n for the n-th reuse).
data Userland = Userland
  { ulId    :: Text
  , ulIndex :: Maybe Int
  } deriving (Eq, Show)

instance ToJSON Userland where
  toJSON u = object [ "id" .= ulId u, "index" .= ulIndex u ]

-- | The core per-step description (StepInfo). @siName@ carries the sleep-until
-- ISO time (Sleep) or the awaited event name (WaitForEvent); it is null for a
-- plain step.run.
data StepInfo = StepInfo
  { siId          :: HashedStepId
  , siOp          :: Opcode
  , siDisplayName :: Text
  , siName        :: Maybe Text
  , siOpts        :: Maybe Value
  , siUserland    :: Maybe Userland
  } deriving (Eq, Show)

-- | Smart constructor for a minimal StepInfo.
stepInfo :: HashedStepId -> Opcode -> Text -> StepInfo
stepInfo i op dn = StepInfo i op dn Nothing Nothing Nothing

-- | Error payload attached to a StepError/StepFailed opcode and to
-- function-level 500 responses.
data ErrorData = ErrorData
  { edCode    :: Text
  , edMessage :: Text
  , edName    :: Text
  , edStack   :: Maybe Text
  } deriving (Eq, Show)

instance ToJSON ErrorData where
  toJSON e = object
    [ "code"    .= edCode e
    , "message" .= edMessage e
    , "name"    .= edName e
    , "stack"   .= edStack e
    ]

-- | One element of the 206 opcode array: a StepInfo merged with an optional
-- success @data@ or an @error@.
data OutgoingOp = OutgoingOp
  { ooStep  :: StepInfo
  , ooData  :: Maybe Value
  , ooError :: Maybe ErrorData
  } deriving (Eq, Show)

-- | Encode an outgoing opcode to the flat wire object the server expects.
encodeOp :: OutgoingOp -> Value
encodeOp (OutgoingOp si md me) = Object $ KM.fromList $
     [ ("id",          toJSON (siId si))
     , ("op",          toJSON (siOp si))
     , ("displayName", toJSON (siDisplayName si))
     , ("name",        toJSON (siName si)) ]
  ++ opt "opts"     (siOpts si)
  ++ opt "userland" (fmap toJSON (siUserland si))
  ++ opt "data"     md
  ++ opt "error"    (fmap toJSON me)
  where
    opt _ Nothing  = []
    opt k (Just v) = [(k, v)]

instance ToJSON OutgoingOp where
  toJSON = encodeOp

--------------------------------------------------------------------------------
-- Duration
--------------------------------------------------------------------------------

-- | A duration in whole milliseconds. Serializes to an Inngest duration string
-- ("30s", "1h", "7d") via 'toDurationStr'.
newtype Duration = Duration { unDuration :: Int }
  deriving (Eq, Ord, Show)

durMillis :: Duration -> Int
durMillis = unDuration

seconds, minutes, hours, days, weeks :: Int -> Duration
seconds n = Duration (n * 1000)
minutes n = Duration (n * 60 * 1000)
hours   n = Duration (n * 3600 * 1000)
days    n = Duration (n * 86400 * 1000)
weeks   n = Duration (n * 604800 * 1000)

-- | Inngest duration string: the largest unit (w/d/h/m/s) that divides the
-- millisecond count evenly. Requires at least one second.
toDurationStr :: Duration -> Either String Text
toDurationStr (Duration ms)
  | ms < 1000 = Left "duration must be at least 1 second"
  | otherwise = go units
  where
    units = [ (604800000, "w"), (86400000, "d"), (3600000, "h")
            , (60000, "m"), (1000, "s") ]
    go [] = Left "duration must be a whole number of seconds"
    go ((u, suf):rest)
      | ms `mod` u == 0 = Right (T.pack (show (ms `div` u)) <> suf)
      | otherwise       = go rest

-- | Millisecond-precision ISO-8601 UTC timestamp, e.g. @2026-09-01T12:34:56.789Z@.
toIsoUtc :: UTCTime -> Text
toIsoUtc t = T.pack (base <> "." <> pad3 millis <> "Z")
  where
    base   = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" t
    picos  = diffTimeToPicoseconds (utctDayTime t)
    millis = fromInteger ((picos `mod` 1000000000000) `div` 1000000000) :: Int
    pad3 n = let s = show n in replicate (3 - length s) '0' <> s

--------------------------------------------------------------------------------
-- Triggers
--------------------------------------------------------------------------------

data Trigger
  = TriggerEvent Text (Maybe Text)   -- ^ event name, optional CEL expression
  | TriggerCron Text (Maybe Text)    -- ^ cron spec, optional jitter
  deriving (Eq, Show)

instance ToJSON Trigger where
  toJSON (TriggerEvent ev mexpr) = object $
    ("event" .= ev) : maybe [] (\e -> ["expression" .= e]) mexpr
  toJSON (TriggerCron cron mjit) = object $
    ("cron" .= cron) : maybe [] (\j -> ["jitter" .= j]) mjit

--------------------------------------------------------------------------------
-- Incoming server request (POST execute body)
--------------------------------------------------------------------------------

data ServerCtx = ServerCtx
  { scAttempt                    :: Int
  , scDisableImmediateExecution  :: Bool
  , scMaxAttempts                :: Maybe Int
  , scRunId                      :: Text
  , scStack                      :: [Text]
  } deriving (Eq, Show)

instance FromJSON ServerCtx where
  parseJSON = withObject "ServerRequestCtx" $ \o -> ServerCtx
    <$> o .:? "attempt" .!= 0
    <*> o .:? "disable_immediate_execution" .!= False
    <*> o .:? "max_attempts"
    <*> o .:? "run_id" .!= ""
    <*> (o .:? "stack" >>= parseStack)
    where
      parseStack :: Maybe Value -> Parser [Text]
      parseStack Nothing         = pure []
      parseStack (Just Null)     = pure []
      parseStack (Just v)        = flip (withObject "stack") v $ \s ->
        s .:? "stack" .!= []

data ServerRequest = ServerRequest
  { srCtx    :: ServerCtx
  , srEvent  :: Event
  , srEvents :: [Event]
  , srSteps  :: Map Text Value
  , srUseApi :: Bool
  } deriving (Eq, Show)

instance FromJSON ServerRequest where
  parseJSON = withObject "ServerRequest" $ \o -> do
    ctx    <- o .:  "ctx"
    event  <- o .:  "event"
    events <- o .:? "events" .!= [event]
    steps  <- o .:? "steps"  .!= mempty
    useApi <- o .:? "use_api" .!= False
    pure (ServerRequest ctx event events steps useApi)
