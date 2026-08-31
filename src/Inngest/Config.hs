{-# LANGUAGE OverloadedStrings #-}

module Inngest.Config
  ( Mode(..)
  , Config(..)
  , devConfig
  , cloudConfig
  ) where

import Data.ByteString (ByteString)
import Data.Text (Text)

data Mode = Dev | Cloud deriving (Eq, Show)

data Config = Config
  { cfgAppId              :: Text
  , cfgMode               :: Mode
  , cfgSigningKey         :: Maybe ByteString
  , cfgSigningKeyFallback :: Maybe ByteString
  , cfgEventKey           :: Maybe ByteString
  , cfgApiOrigin          :: Text
  , cfgEventApiOrigin     :: Text
  , cfgServePath          :: Text
  } deriving (Eq, Show)

devConfig :: Text -> Config
devConfig appId = Config
  { cfgAppId = appId, cfgMode = Dev
  , cfgSigningKey = Nothing, cfgSigningKeyFallback = Nothing, cfgEventKey = Nothing
  , cfgApiOrigin = "http://127.0.0.1:8288"
  , cfgEventApiOrigin = "http://127.0.0.1:8288"
  , cfgServePath = "/api/inngest"
  }

cloudConfig :: Text -> Config
cloudConfig appId = (devConfig appId)
  { cfgMode = Cloud
  , cfgApiOrigin = "https://api.inngest.com"
  , cfgEventApiOrigin = "https://inn.gs"
  }
