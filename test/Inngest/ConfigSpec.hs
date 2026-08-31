{-# LANGUAGE OverloadedStrings #-}
module Inngest.ConfigSpec (spec) where

import Test.Hspec
import Inngest.Config

spec :: Spec
spec = describe "Config" $ do
  it "dev config uses the dev server origins and default serve path" $ do
    let c = devConfig "my-app"
    cfgMode c            `shouldBe` Dev
    cfgApiOrigin c       `shouldBe` "http://127.0.0.1:8288"
    cfgEventApiOrigin c  `shouldBe` "http://127.0.0.1:8288"
    cfgServePath c       `shouldBe` "/api/inngest"

  it "cloud config uses cloud origins" $ do
    let c = cloudConfig "my-app"
    cfgMode c            `shouldBe` Cloud
    cfgApiOrigin c       `shouldBe` "https://api.inngest.com"
    cfgEventApiOrigin c  `shouldBe` "https://inn.gs"
