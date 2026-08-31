{-# LANGUAGE OverloadedStrings #-}
module Inngest.TypesSpec (spec) where

import Test.Hspec
import Data.Aeson (toJSON, object, (.=), Value(..))
import Inngest.Types

spec :: Spec
spec = describe "Event JSON" $ do
  it "encodes a minimal event without optional fields" $
    toJSON (mkEvent "app/created" (object ["x" .= (1 :: Int)]))
      `shouldBe` object [ "name" .= ("app/created" :: String)
                        , "data" .= object ["x" .= (1 :: Int)] ]

  it "includes id, ts and v when present" $
    toJSON (mkEvent "app/created" (object []))
             { eventId = Just "evt_1", eventTs = Just 1700, eventVer = Just "1" }
      `shouldBe` object [ "name" .= ("app/created" :: String)
                        , "data" .= object []
                        , "id"   .= ("evt_1" :: String)
                        , "ts"   .= (1700 :: Int)
                        , "v"    .= ("1" :: String) ]
