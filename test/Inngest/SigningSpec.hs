{-# LANGUAGE OverloadedStrings #-}
module Inngest.SigningSpec (spec) where

import Test.Hspec
import Inngest.Signing
import Data.Aeson (Value, eitherDecodeFileStrict, withObject, (.:), FromJSON(..), decodeStrict)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Maybe (fromJust)

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

  describe "HMAC signing" $ do
    let key = "signkey-test-deadbeef"
        raw = "{\"b\":1,\"a\":2}"                       -- non-canonical field order
        body = fromJust (decodeStrict raw) :: Value

    it "round-trips: a response we sign verifies with the same key over raw bytes" $ do
      let hdr = signResponse key 1700000000 raw
      verifyRaw [key] raw hdr `shouldBe` Just key

    it "verifies a request signature computed over the canonical body" $ do
      -- server-side signature is HMAC over canonical form + timestamp
      let t   = 1700000000
          hdr = signCanonical key t body
      verifyRequest [key] raw hdr `shouldBe` Just key

    it "falls back to the secondary key" $ do
      let good = "signkey-test-deadbeef"; bad = "signkey-test-0000"
          hdr = signResponse good 1700000000 raw
      verifyRaw [bad, good] raw hdr `shouldBe` Just good

    it "rejects a tampered body" $ do
      let hdr = signResponse key 1700000000 raw
      verifyRaw [key] "{\"a\":2,\"b\":3}" hdr `shouldBe` Nothing

data JcsCase = JcsCase Value Text
instance FromJSON JcsCase where
  parseJSON = withObject "JcsCase" $ \o -> JcsCase <$> o .: "input" <*> o .: "canonical"
