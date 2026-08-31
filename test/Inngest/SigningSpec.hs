{-# LANGUAGE OverloadedStrings #-}
module Inngest.SigningSpec (spec) where

import Test.Hspec
import Inngest.Signing
import Data.Aeson (Value, eitherDecodeFileStrict, withObject, (.:), FromJSON(..))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)

spec :: Spec
spec = do
  describe "key hashing" $ do
    it "strips the signkey prefix" $
      stripKeyPrefix "signkey-test-abcdef" `shouldBe` "abcdef"

    it "hashes a signing key as sha256 of the hex-decoded remainder" $
      hashSigningKey "signkey-test-abcdef"
        `shouldBe` "995da3cf545787d65f9ced52674e92ee8171c87c7a4008aa4349ec47d21609a7"

    it "hashes an event key as sha256 of utf8" $
      hashEventKey "my-event-key"
        `shouldBe` "756db3455bd70816c089ed7fcd514ab0e66cf5cdf62ccb7a3bca66d96ee3132c"

  describe "JCS canonicalization" $
    it "matches the reference jcs library on all golden vectors" $ do
      Right vs <- eitherDecodeFileStrict "test/fixtures/jcs/vectors.json"
                    :: IO (Either String [JcsCase])
      mapM_ (\(JcsCase inp expected) ->
               canonicalize inp `shouldBe` encodeUtf8 expected) vs

data JcsCase = JcsCase Value Text
instance FromJSON JcsCase where
  parseJSON = withObject "JcsCase" $ \o -> JcsCase <$> o .: "input" <*> o .: "canonical"
