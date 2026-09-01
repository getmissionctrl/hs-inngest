{-# LANGUAGE OverloadedStrings #-}
module Inngest.ServeSpec (spec) where

import Test.Hspec
import Data.Aeson (Value(..), decode, toJSON)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import Inngest.Config
import Inngest.Types (Trigger(..))
import Inngest.Step (stepRun)
import Inngest.Function
import Inngest.Serve.Servant

cfg :: Config
cfg = devConfig "app"

fns :: [Function IO]
fns =
  [ createFunction (defaultFnOpts "f") [TriggerEvent "app/created" Nothing]
      (\_ _ -> pure (42 :: Int))
  , createFunction (defaultFnOpts "steps") [TriggerEvent "app/created" Nothing]
      (\_ _ -> stepRun "a" (pure (1 :: Int)))
  ]

input :: ServeMethod -> [(Text, Maybe Text)] -> [(Text, Text)] -> BL.ByteString -> ServeInput
input = ServeInput

run :: ServeInput -> IO ServeOutput
run = handleInngest cfg "https://x/api/inngest" fns

execBody :: BL.ByteString
execBody = "{\"ctx\":{\"run_id\":\"r\",\"attempt\":0,\"stack\":{\"stack\":[]}},\
           \\"event\":{\"name\":\"app/created\",\"data\":{}},\"steps\":{},\"use_api\":false}"

hdr :: ServeOutput -> Text -> Maybe Text
hdr out k = lookup k (soHeaders out)

spec :: Spec
spec = do
  describe "GET introspection" $ do
    it "returns an unauthenticated inspection in dev mode" $ do
      out <- run (input MGET [] [] "")
      soStatus out `shouldBe` 200
      case decode (soBody out) :: Maybe Value of
        Just (Object o) -> do
          KM.lookup "schema_version" o `shouldBe` Just (String "2024-05-24")
          KM.lookup "mode" o           `shouldBe` Just (String "dev")
          KM.lookup "function_count" o `shouldBe` Just (toJSON (2 :: Int))
        _ -> expectationFailure "expected an inspection object"

    it "403s on a server-kind mismatch" $ do
      out <- run (input MGET [] [("x-inngest-server-kind", "cloud")] "")
      soStatus out `shouldBe` 403

  describe "POST execute" $ do
    it "fast-paths probe=trust to an empty 200" $ do
      out <- run (input MPOST [("probe", Just "trust")] [] "")
      soStatus out `shouldBe` 200
      soBody out   `shouldBe` ""

    it "400s when fnId is missing" $ do
      out <- run (input MPOST [] [] execBody)
      soStatus out `shouldBe` 400

    it "runs a function to completion (200 + bare output)" $ do
      out <- run (input MPOST [("fnId", Just "app-f")] [] execBody)
      soStatus out `shouldBe` 200
      decode (soBody out) `shouldBe` Just (toJSON (42 :: Int))

    it "returns 206 with an opcode array for a stepped function" $ do
      out <- run (input MPOST [("fnId", Just "app-steps")] [] execBody)
      soStatus out `shouldBe` 206
      case decode (soBody out) :: Maybe Value of
        Just (Array _) -> pure ()
        _              -> expectationFailure "expected a 206 array"

    it "carries the standard header block" $ do
      out <- run (input MPOST [("fnId", Just "app-f")] [] execBody)
      hdr out "x-inngest-sdk"          `shouldBe` Just "inngest-haskell:v0.1.0"
      hdr out "x-inngest-req-version"  `shouldBe` Just "2"

  describe "PUT sync" $
    it "401s an in-band cloud sync without a signature" $ do
      let cloud = cloudConfig "app"
      out <- handleInngest cloud "https://x/api/inngest" fns
               (input MPUT [] [("x-inngest-sync-kind", "in_band")] "{}")
      soStatus out `shouldBe` 401
