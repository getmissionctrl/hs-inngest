{-# LANGUAGE OverloadedStrings #-}

module Inngest.Types
  ( Event(..)
  , mkEvent
  , EventId
  ) where

import Data.Aeson
import Data.Text (Text)
import qualified Data.Aeson.KeyMap as KM

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
