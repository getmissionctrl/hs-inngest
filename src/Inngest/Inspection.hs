{-# LANGUAGE OverloadedStrings #-}

-- | Introspection bodies returned by @GET@ on the serve endpoint (spec §7.1) and
-- embedded in the in-band sync response.
module Inngest.Inspection
  ( modeText
  , unauthenticatedInspection
  , authenticatedInspection
  ) where

import Data.Aeson (Value, object, (.=), toJSON)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

import Inngest.Config
import Inngest.Const (schemaVersion, capabilities, sdkLanguage, sdkVersion)
import Inngest.Signing (hashSigningKey, hashEventKey)

modeText :: Mode -> Text
modeText Dev   = "dev"
modeText Cloud = "cloud"

hashText :: (a -> b) -> (b -> Text) -> Maybe a -> Maybe Text
hashText h dec = fmap (dec . h)

-- | The inspection body returned when the request is not (validly) signed.
unauthenticatedInspection :: Config -> Int -> Value
unauthenticatedInspection cfg fnCount = object
  [ "schema_version"           .= schemaVersion
  , "function_count"           .= fnCount
  , "has_event_key"            .= present (cfgEventKey cfg)
  , "has_signing_key"          .= present (cfgSigningKey cfg)
  , "has_signing_key_fallback" .= present (cfgSigningKeyFallback cfg)
  , "mode"                     .= modeText (cfgMode cfg)
  ]
  where present = maybe False (const True)

-- | The richer inspection body returned when the request is validly signed.
authenticatedInspection :: Config -> Int -> Value
authenticatedInspection cfg fnCount = object
  [ "schema_version"            .= schemaVersion
  , "api_origin"                .= cfgApiOrigin cfg
  , "app_id"                    .= cfgAppId cfg
  , "authentication_succeeded"  .= True
  , "capabilities"              .= capabilities
  , "env"                       .= (Nothing :: Maybe Text)
  , "event_api_origin"          .= cfgEventApiOrigin cfg
  , "event_key_hash"            .= hashText hashEventKey decodeUtf8 (cfgEventKey cfg)
  , "framework"                 .= ("servant" :: Text)
  , "function_count"            .= fnCount
  , "has_event_key"             .= present (cfgEventKey cfg)
  , "has_signing_key"           .= present (cfgSigningKey cfg)
  , "has_signing_key_fallback"  .= present (cfgSigningKeyFallback cfg)
  , "mode"                      .= modeText (cfgMode cfg)
  , "sdk_language"              .= sdkLanguage
  , "sdk_version"               .= sdkVersion
  , "serve_origin"              .= (Nothing :: Maybe Text)
  , "serve_path"                .= toJSON (cfgServePath cfg)
  , "signing_key_fallback_hash" .= hashText hashSigningKey decodeUtf8 (cfgSigningKeyFallback cfg)
  , "signing_key_hash"          .= hashText hashSigningKey decodeUtf8 (cfgSigningKey cfg)
  ]
  where present = maybe False (const True)
