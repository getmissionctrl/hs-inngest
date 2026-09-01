{-# LANGUAGE OverloadedStrings #-}
module Inngest.ClientSpec (spec) where

import Test.Hspec
import Data.Aeson (object, (.=), eitherDecode)
import qualified Data.ByteString as BS
import Network.HTTP.Client (method, path, requestBody, requestHeaders, RequestBody(..))
import Inngest.Config
import Inngest.Types
import Inngest.Client

spec :: Spec
spec = do
  describe "event send request" $ do
    let cfg = (devConfig "app") { cfgEventKey = Just "ekey" }
        evs = [ mkEvent "a/one" (object ["n" .= (1 :: Int)]) ]

    it "targets POST /e/<key>" $ do
      req <- buildSendRequest cfg evs
      method req `shouldBe` "POST"
      path req   `shouldBe` "/e/ekey"

    it "serializes the body as a JSON array of events" $ do
      req <- buildSendRequest cfg evs
      case requestBody req of
        RequestBodyLBS bs -> eitherDecode bs `shouldBe` Right [encodedEvent]
        _                 -> expectationFailure "expected an LBS body"

    it "parses the send response ids" $
      parseSendResponse "{\"ids\":[\"01H\",\"02H\"]}" `shouldBe` Right ["01H","02H"]

  describe "use_api requests" $ do
    let cfg = (cloudConfig "app") { cfgSigningKey = Just "signkey-test-abcdef" }

    it "builds a Bearer-authed GET to /v0/runs/<id>/batch" $ do
      req <- buildUseApiRequest cfg "run1" "batch"
      method req `shouldBe` "GET"
      path req   `shouldBe` "/v0/runs/run1/batch"
      lookup "Authorization" (requestHeaders req)
        `shouldSatisfy` maybe False (BS.isPrefixOf "Bearer ")

    it "targets /actions for step results" $ do
      req <- buildUseApiRequest cfg "run1" "actions"
      path req `shouldBe` "/v0/runs/run1/actions"

    it "parses a batch response as an array of events" $
      fmap length (parseBatchResponse "[{\"name\":\"e\",\"data\":{}}]") `shouldBe` Right 1
  where
    encodedEvent = object [ "name" .= ("a/one" :: String)
                          , "data" .= object ["n" .= (1 :: Int)] ]
