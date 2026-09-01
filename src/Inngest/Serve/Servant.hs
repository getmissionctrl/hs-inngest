{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}

-- | The HTTP serve layer (spec §7.1). The framework-agnostic core is
-- 'handleInngest' (method/query/headers/body → status/headers/body); 'toApplication'
-- lifts it to a WAI 'Application', and 'InngestAPI'/'inngestServer' mount it in a
-- Servant tree (as a 'Raw' endpoint).
module Inngest.Serve.Servant
  ( -- * Framework-agnostic core
    ServeMethod(..)
  , ServeInput(..)
  , ServeOutput(..)
  , handleInngest
    -- * WAI
  , toApplication
    -- * Servant
  , InngestAPI
  , inngestServer
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Aeson (Value(..), encode, eitherDecode, object, (.=))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8, decodeUtf8')
import Data.Tagged (Tagged(..))
import qualified Data.CaseInsensitive as CI
import Data.Time.Clock.POSIX (getPOSIXTime)
import qualified Network.HTTP.Types as H
import Network.Wai (Application, Response, responseLBS, requestMethod, queryString, requestHeaders, strictRequestBody)
import qualified Network.HTTP.Client as HC
import Network.HTTP.Client.TLS (getGlobalManager)
import Servant (Server)
import Servant.API (Raw)

import Inngest.Config
import Inngest.Const
import Inngest.Types (ServerRequest, HashedStepId)
import Inngest.Step (MonadInngest)
import Inngest.Function
import Inngest.Execution
import Inngest.Inspection
import Inngest.Sync
import Inngest.Signing (verifyRequest, signResponse)

--------------------------------------------------------------------------------
-- Framework-agnostic request/response
--------------------------------------------------------------------------------

data ServeMethod = MGET | MPOST | MPUT | MOther
  deriving (Eq, Show)

data ServeInput = ServeInput
  { siMethod  :: ServeMethod
  , siQuery   :: [(Text, Maybe Text)]  -- ^ fnId, stepId, probe, deployId
  , siHeaders :: [(Text, Text)]        -- ^ lower-cased header name -> value
  , siBody    :: BL.ByteString
  } deriving (Show)

data ServeOutput = ServeOutput
  { soStatus  :: Int
  , soHeaders :: [(Text, Text)]
  , soBody    :: BL.ByteString
  } deriving (Show)

qLookup :: Text -> ServeInput -> Maybe Text
qLookup k si = join (lookup k (siQuery si))

hLookup :: Text -> ServeInput -> Maybe Text
hLookup k si = lookup (T.toLower k) (siHeaders si)

--------------------------------------------------------------------------------
-- The core handler
--------------------------------------------------------------------------------

baseHeaders :: Config -> [(Text, Text)]
baseHeaders cfg =
  [ ("x-inngest-sdk", sdkHeaderValue)
  , ("x-inngest-req-version", preferredExecutionVersion)
  , ("x-inngest-sdk-handled", "true")
  , ("x-inngest-expected-server-kind", modeText (cfgMode cfg))
  , ("server-timing", "handler")
  , ("content-type", "application/json")
  ]

signingKeys :: Config -> [BS.ByteString]
signingKeys cfg = catMaybes [cfgSigningKey cfg, cfgSigningKeyFallback cfg]

-- | Validate a request signature. @Nothing@ = invalid (401). @Just mkey@ =
-- accepted, with @mkey@ the validating key (@Nothing@ in dev mode, which skips
-- verification).
validateReq :: Config -> BL.ByteString -> ServeInput -> Maybe (Maybe BS.ByteString)
validateReq cfg body si = case cfgMode cfg of
  Dev   -> Just Nothing
  Cloud -> case hLookup "x-inngest-signature" si of
    Nothing  -> Nothing
    Just sig -> fmap Just (verifyRequest (signingKeys cfg) (BL.toStrict body) (encodeUtf8 sig))

-- | The key a valid response should be signed with, if any.
keyToSign :: Maybe (Maybe BS.ByteString) -> Maybe BS.ByteString
keyToSign = join

resolveFn :: Config -> Text -> [Function m] -> Maybe (Function m, Bool)
resolveFn cfg fnId fns = foldr (\x acc -> maybe acc Just x) Nothing (map match fns)
  where
    appId = cfgAppId cfg
    match fn =
      let fq = appId <> "-" <> foId (fnOpts fn)
      in if fnId == fq                     then Just (fn, False)
         else if fnId == fq <> "-failure"  then Just (fn, True)
         else Nothing

-- | Handle one serve request against an app's config, serve URL, and functions.
handleInngest
  :: MonadInngest m
  => Config
  -> Text            -- ^ public serve URL (for sync + function config URLs)
  -> [Function m]
  -> ServeInput
  -> m ServeOutput
handleInngest cfg appUrl fns si = case siMethod si of
  MGET   -> handleGet cfg fns si
  MPOST  -> handlePost cfg fns si
  MPUT   -> handlePut cfg appUrl fns si
  MOther -> pure (ServeOutput 405 (baseHeaders cfg) "")

--------------------------------------------------------------------------------
-- GET — introspection
--------------------------------------------------------------------------------

handleGet :: MonadInngest m => Config -> [Function m] -> ServeInput -> m ServeOutput
handleGet cfg fns si
  | mismatch  = pure (ServeOutput 403 (baseHeaders cfg) "{}")
  | otherwise = do
      let mkey  = validateReq cfg (siBody si) si
          authed = case mkey of Just (Just _) -> True; _ -> False
          body  = if authed then authenticatedInspection cfg n
                            else unauthenticatedInspection cfg n
      signOut (keyToSign mkey) (ServeOutput 200 (baseHeaders cfg) (encode body))
  where
    n = length fns
    mismatch = case hLookup "x-inngest-server-kind" si of
      Just k  -> k /= modeText (cfgMode cfg)
      Nothing -> False

--------------------------------------------------------------------------------
-- POST — execute
--------------------------------------------------------------------------------

handlePost :: MonadInngest m => Config -> [Function m] -> ServeInput -> m ServeOutput
handlePost cfg fns si
  | qLookup "probe" si == Just "trust" =
      pure (ServeOutput 200 (baseHeaders cfg) "")
  | otherwise = case qLookup "fnId" si of
      Nothing -> pure (errorOut cfg 400 "query_param_missing" "missing fnId")
      Just fnId -> case validateReq cfg (siBody si) si of
        Nothing   -> pure unauthorizedOut
        Just mkey -> case resolveFn cfg fnId fns of
          Nothing -> pure (errorOut cfg 500 "function_not_found" ("function not found: " <> fnId))
          Just (fn, isFailure) -> case eitherDecode (siBody si) :: Either String ServerRequest of
            Left e   -> pure (errorOut cfg 500 "unknown" (T.pack e))
            Right sr -> do
              let handler = if isFailure then fromMaybe (fnHandler fn) (fnOnFailure fn)
                                         else fnHandler fn
              res <- executeFunction (fn { fnHandler = handler }) sr (targetOf si)
              let hdrs = baseHeaders cfg
                       ++ [ ("x-inngest-no-retry", "true") | erNoRetry res ]
                       ++ maybe [] (\t -> [("retry-after", tShow t)]) (erRetryAfter res)
              signOut (keyToSign (Just mkey)) (ServeOutput (erStatus res) hdrs (renderBody res))

-- Under stepId targeting, the query value is the hashed target id ("step" = root).
targetOf :: ServeInput -> Maybe HashedStepId
targetOf si = case qLookup "stepId" si of
  Just s | s /= "step" && not (T.null s) -> Just s
  _                                      -> Nothing

--------------------------------------------------------------------------------
-- PUT — sync
--------------------------------------------------------------------------------

handlePut :: MonadInngest m => Config -> Text -> [Function m] -> ServeInput -> m ServeOutput
handlePut cfg appUrl fns si
  | inBand = case validateReq cfg (siBody si) si of
      Just (Just k) ->
        let body = inBandResponseBody (cfgAppId cfg) appUrl cfgs
                     (authenticatedInspection cfg (length fns))
            hdrs = baseHeaders cfg ++ [("x-inngest-sync-kind", "in_band")]
        in signOut (Just k) (ServeOutput 200 hdrs (encode body))
      _ -> pure unauthorizedOut
  | otherwise = do
      reqResult <- liftIO (registerOutOfBand cfg appUrl cfgs (qLookup "deployId" si))
      pure $ case reqResult of
        Left err -> errorOut cfg 500 "registration_failed" (T.pack err)
        Right rb -> ServeOutput 200
                      (baseHeaders cfg ++ [("x-inngest-sync-kind", "out_of_band")]) rb
  where
    cfgs   = configsOf cfg appUrl fns
    inBand = cfgMode cfg == Cloud
             && hLookup "x-inngest-sync-kind" si == Just "in_band"

registerOutOfBand :: Config -> Text -> [Value] -> Maybe Text -> IO (Either String BL.ByteString)
registerOutOfBand cfg appUrl cfgs deployId = do
  mgr <- getGlobalManager
  req <- buildRegisterRequest cfg appUrl cfgs deployId
  eresp <- try (HC.httpLbs req mgr) :: IO (Either SomeException (HC.Response BL.ByteString))
  pure $ case eresp of
    Left e     -> Left (show e)
    Right resp -> Right (HC.responseBody resp)

--------------------------------------------------------------------------------
-- Shared helpers
--------------------------------------------------------------------------------

configsOf :: Config -> Text -> [Function m] -> [Value]
configsOf cfg appUrl = concatMap (functionConfigs (cfgAppId cfg) appUrl)

errorOut :: Config -> Int -> Text -> Text -> ServeOutput
errorOut cfg status code msg =
  ServeOutput status (baseHeaders cfg) (encode (object ["code" .= code, "message" .= msg]))

unauthorizedOut :: ServeOutput
unauthorizedOut =
  ServeOutput 401
    [ ("content-type", "application/json"), ("x-inngest-sdk-handled", "true") ]
    (encode (object ["message" .= ("Unauthorized" :: Text)]))

-- | Sign a response body (raw bytes) when a signing key is available.
signOut :: MonadIO m => Maybe BS.ByteString -> ServeOutput -> m ServeOutput
signOut Nothing  out = pure out
signOut (Just k) out = do
  t <- liftIO (round <$> getPOSIXTime)
  let sig = signResponse k t (BL.toStrict (soBody out))
  pure out { soHeaders = soHeaders out ++ [("x-inngest-signature", decodeUtf8 sig)] }

tShow :: Show a => a -> Text
tShow = T.pack . show

--------------------------------------------------------------------------------
-- WAI + Servant
--------------------------------------------------------------------------------

-- | Lift the core handler into a WAI 'Application'. The natural transformation
-- runs the user's base monad @m@ down to 'IO'.
toApplication
  :: MonadInngest m
  => (forall a. m a -> IO a)
  -> Config
  -> Text
  -> [Function m]
  -> Application
toApplication runM cfg appUrl fns req respond = do
  body <- strictRequestBody req
  let si = ServeInput
        { siMethod  = methodOf (requestMethod req)
        , siQuery   = map decodeQ (queryString req)
        , siHeaders = map decodeH (requestHeaders req)
        , siBody    = body
        }
  out <- runM (handleInngest cfg appUrl fns si)
  respond (toResponse out)
  where
    methodOf m
      | m == H.methodGet  = MGET
      | m == H.methodPost = MPOST
      | m == H.methodPut  = MPUT
      | otherwise         = MOther
    decodeQ (k, mv) = (decodeLenient k, fmap decodeLenient mv)
    decodeH (k, v)  = (T.toLower (decodeLenient (CI.original k)), decodeLenient v)
    decodeLenient   = either (const "") id . decodeUtf8'

toResponse :: ServeOutput -> Response
toResponse out =
  responseLBS
    (H.mkStatus (soStatus out) "")
    (map (\(k, v) -> (CI.mk (encodeUtf8 k), encodeUtf8 v)) (soHeaders out))
    (soBody out)

-- | The Inngest serve endpoint, mountable in a Servant API tree.
type InngestAPI = Raw

-- | A Servant server for 'InngestAPI' that delegates to the WAI application.
inngestServer
  :: MonadInngest m
  => (forall a. m a -> IO a)
  -> Config
  -> Text
  -> [Function m]
  -> Server InngestAPI
inngestServer runM cfg appUrl fns = Tagged (toApplication runM cfg appUrl fns)
