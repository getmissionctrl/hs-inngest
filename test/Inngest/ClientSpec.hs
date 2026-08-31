{-# LANGUAGE OverloadedStrings #-}
module Inngest.ClientSpec (spec) where

import Test.Hspec
import Data.Aeson (object, (.=), encode, eitherDecode)
import Network.HTTP.Client (method, path, requestBody, RequestBody(..))
import Inngest.Config
import Inngest.Types
import Inngest.Client

spec :: Spec
spec = describe "event send request" $ do
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
  where
    encodedEvent = object [ "name" .= ("a/one" :: String)
                          , "data" .= object ["n" .= (1 :: Int)] ]
