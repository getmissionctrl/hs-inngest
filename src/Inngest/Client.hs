{-# LANGUAGE OverloadedStrings #-}

module Inngest.Client
  ( buildSendRequest
  , parseSendResponse
  , send
  ) where

import Data.Aeson (encode, eitherDecode, toJSON, withObject, (.:))
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client
import Network.HTTP.Client.TLS (getGlobalManager)
import Inngest.Config
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
