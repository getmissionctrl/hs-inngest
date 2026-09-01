{-# LANGUAGE OverloadedStrings #-}
module Inngest.SyncSpec (spec) where

import Test.Hspec
import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Network.HTTP.Client (method, path, requestHeaders, queryString)
import Inngest.Config
import Inngest.Sync

spec :: Spec
spec = do
  describe "deepStripNull" $
    it "recursively drops null-valued keys" $
      deepStripNull (object [ "a" .= (1 :: Int), "b" .= Null
                            , "nested" .= object ["keep" .= True, "drop" .= Null] ])
        `shouldBe` object [ "a" .= (1 :: Int), "nested" .= object ["keep" .= True] ]

  describe "synchronizeRequestBody" $
    it "has appname/deploy_type/sdk/v and drops an absent framework" $
      case synchronizeRequestBody "my-app" "https://x/api/inngest" Nothing [] of
        Object o -> do
          KM.lookup "appname"     o `shouldBe` Just (String "my-app")
          KM.lookup "deploy_type" o `shouldBe` Just (String "ping")
          KM.lookup "v"           o `shouldBe` Just (String "0.1")
          KM.lookup "sdk"         o `shouldBe` Just (String "haskell:v0.1.0")
          KM.member "framework"   o `shouldBe` False   -- stripped (was null)
        _ -> expectationFailure "expected an object"

  describe "buildRegisterRequest" $ do
    let cfg = (cloudConfig "my-app") { cfgSigningKey = Just "signkey-test-abcdef" }

    it "POSTs to /fn/register with Bearer auth" $ do
      req <- buildRegisterRequest cfg "https://x/api/inngest" [] Nothing
      method req `shouldBe` "POST"
      path req   `shouldBe` "/fn/register"
      lookup "Authorization" (requestHeaders req)
        `shouldSatisfy` maybe False (BS.isPrefixOf "Bearer ")

    it "passes deployId through as a query param" $ do
      req <- buildRegisterRequest cfg "https://x/api/inngest" [] (Just "dep_1")
      queryString req `shouldSatisfy` BS.isInfixOf "deployId=dep_1"
