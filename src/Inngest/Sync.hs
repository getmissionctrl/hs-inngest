{-# LANGUAGE OverloadedStrings #-}

-- | App registration: the out-of-band @POST /fn/register@ request and the
-- in-band synchronize response (spec §7.2). Both strip null fields before
-- sending (the Inngest server's Go marshalling rejects unexpected nulls).
module Inngest.Sync
  ( deepStripNull
  , synchronizeRequestBody
  , inBandResponseBody
  , buildRegisterRequest
  ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client

import Inngest.Config
import Inngest.Const
import Inngest.Signing (hashSigningKey)

-- | Recursively remove object keys whose value is @null@.
deepStripNull :: Value -> Value
deepStripNull (Object o) = Object (KM.mapMaybe strip o)
  where strip Null = Nothing
        strip v    = Just (deepStripNull v)
deepStripNull (Array a)  = Array (fmap deepStripNull a)
deepStripNull v          = v

-- | The out-of-band SynchronizeRequest body (null fields stripped).
synchronizeRequestBody :: Text -> Text -> Maybe Text -> [Value] -> Value
synchronizeRequestBody appId url framework fns = deepStripNull $ object
  [ "appname"      .= appId
  , "framework"    .= framework
  , "functions"    .= fns
  , "sdk"          .= syncSdkValue
  , "url"          .= url
  , "v"            .= ("0.1" :: Text)
  , "deploy_type"  .= ("ping" :: Text)
  , "capabilities" .= capabilities
  ]

-- | The in-band synchronize response body (null fields stripped). @inspection@
-- is the authenticated inspection object.
inBandResponseBody :: Text -> Text -> [Value] -> Value -> Value
inBandResponseBody appId url fns inspection = deepStripNull $ object
  [ "app_id"       .= appId
  , "framework"    .= (Nothing :: Maybe Text)
  , "functions"    .= fns
  , "inspection"   .= inspection
  , "platform"     .= (Nothing :: Maybe Text)
  , "sdk_author"   .= sdkAuthor
  , "sdk_language" .= sdkLanguage
  , "sdk_version"  .= sdkVersion
  , "url"          .= url
  ]

-- | Build the outbound @POST {api_origin}/fn/register@ request:
-- Bearer-authed with the hashed signing key, optional @deployId@ passthrough.
buildRegisterRequest :: Config -> Text -> [Value] -> Maybe Text -> IO Request
buildRegisterRequest cfg url fns mDeployId = do
  req0 <- parseRequest (T.unpack (cfgApiOrigin cfg) <> "/fn/register")
  let body = synchronizeRequestBody (cfgAppId cfg) url Nothing fns
      q    = maybe [] (\d -> [("deployId", Just (encodeUtf8 d))]) mDeployId
  pure $ setQueryString q req0
    { method         = "POST"
    , requestHeaders = auth ++ [("Content-Type", "application/json")]
    , requestBody    = RequestBodyLBS (encode body)
    }
  where
    auth = maybe [] (\k -> [("Authorization", "Bearer " <> hashSigningKey k)]) (cfgSigningKey cfg)
