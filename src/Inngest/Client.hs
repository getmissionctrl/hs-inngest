{-# LANGUAGE OverloadedStrings #-}

module Inngest.Client
  ( buildSendRequest
  , parseSendResponse
  , send
    -- * use_api fetches (oversized payloads)
  , buildUseApiRequest
  , parseBatchResponse
  , parseStepsResponse
  , fetchBatch
  , fetchSteps
  ) where

import Data.Aeson (encode, eitherDecode, toJSON, withObject, (.:), Value)
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS (getGlobalManager)
import Inngest.Config
import Inngest.Signing (hashSigningKey)
import Inngest.Types

-- | Build the (unsigned) HTTP request that posts events to the event API.
buildSendRequest :: Config -> [Event] -> IO Request
buildSendRequest cfg evs = do
  let key = maybe "" id (cfgEventKey cfg)
      url = T.unpack (cfgEventApiOrigin cfg) <> "/e/" <> BC.unpack key
  req0 <- parseRequest url
  pure req0
    { method = "POST"
    , requestHeaders = [("Content-Type", "application/json")]
    , requestBody = RequestBodyLBS (encode (map toJSON evs))
    }

-- | Parse @{"ids":[...]}@ from a send response body.
parseSendResponse :: BL.ByteString -> Either String [Text]
parseSendResponse = fmap ids . eitherDecode
  where ids = unSendResult

newtype SendResult = SendResult { unSendResult :: [Text] }
instance A.FromJSON SendResult where
  parseJSON = withObject "SendResult" $ \o -> SendResult <$> o .: "ids"

-- | Send events, returning the assigned ids.
send :: Config -> [Event] -> IO (Either String [Text])
send cfg evs = do
  mgr <- getGlobalManager
  req <- buildSendRequest cfg evs
  resp <- httpLbs req mgr
  pure (parseSendResponse (responseBody resp))

--------------------------------------------------------------------------------
-- use_api fetches
--------------------------------------------------------------------------------

-- | Build a Bearer-authed GET to the run API. @leaf@ is @"batch"@ (events) or
-- @"actions"@ (steps). Auth uses the SHA256-hashed signing key.
buildUseApiRequest :: Config -> Text -> Text -> IO Request
buildUseApiRequest cfg runId leaf = do
  let url = T.unpack (cfgApiOrigin cfg) <> "/v0/runs/" <> T.unpack runId <> "/" <> T.unpack leaf
  req0 <- parseRequest url
  pure req0
    { method = "GET"
    , requestHeaders = auth ++ [("Content-Type", "application/json")]
    }
  where
    auth = case cfgSigningKey cfg of
      Nothing -> []
      Just k  -> [("Authorization", "Bearer " <> hashSigningKey k)]

-- | Parse the batch endpoint's response — a JSON array of events.
parseBatchResponse :: BL.ByteString -> Either String [Event]
parseBatchResponse = eitherDecode

-- | Parse the actions endpoint's response — a map of hashed step id to result.
parseStepsResponse :: BL.ByteString -> Either String (Map Text Value)
parseStepsResponse = eitherDecode

-- | Fetch the batch of triggering events for a run.
fetchBatch :: Config -> Text -> IO (Either String [Event])
fetchBatch cfg runId = do
  mgr  <- getGlobalManager
  req  <- buildUseApiRequest cfg runId "batch"
  resp <- httpLbs req mgr
  pure (parseBatchResponse (responseBody resp))

-- | Fetch the memoized step results for a run.
fetchSteps :: Config -> Text -> IO (Either String (Map Text Value))
fetchSteps cfg runId = do
  mgr  <- getGlobalManager
  req  <- buildUseApiRequest cfg runId "actions"
  resp <- httpLbs req mgr
  pure (parseStepsResponse (responseBody resp))
