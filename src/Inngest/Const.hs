{-# LANGUAGE OverloadedStrings #-}

-- | SDK-wide constants: version/language identifiers, header values, and the
-- capability/inspection schema strings.
module Inngest.Const
  ( sdkLanguage
  , sdkVersion
  , sdkAuthor
  , sdkHeaderValue
  , syncSdkValue
  , preferredExecutionVersion
  , schemaVersion
  , capabilities
  ) where

import Data.Aeson (Value, object, (.=))
import Data.Text (Text)

sdkLanguage :: Text
sdkLanguage = "haskell"

sdkVersion :: Text
sdkVersion = "0.1.0"

sdkAuthor :: Text
sdkAuthor = "inngest"

-- | @x-inngest-sdk@ / user-agent value, e.g. @inngest-haskell:v0.1.0@.
sdkHeaderValue :: Text
sdkHeaderValue = "inngest-" <> sdkLanguage <> ":v" <> sdkVersion

-- | The @sdk@ field in a SynchronizeRequest, e.g. @haskell:v0.1.0@ (no prefix).
syncSdkValue :: Text
syncSdkValue = sdkLanguage <> ":v" <> sdkVersion

-- | @x-inngest-req-version@.
preferredExecutionVersion :: Text
preferredExecutionVersion = "2"

-- | Inspection schema version.
schemaVersion :: Text
schemaVersion = "2024-05-24"

-- | Advertised capabilities (introspection + sync).
capabilities :: Value
capabilities = object
  [ "connect"      .= ("v1" :: Text)
  , "in_band_sync" .= ("v1" :: Text)
  , "trust_probe"  .= ("v1" :: Text)
  ]
