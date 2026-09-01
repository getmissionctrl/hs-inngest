{-# LANGUAGE OverloadedStrings #-}
module Inngest.ExecutionSpec (spec) where

import Test.Hspec
import Data.Aeson (Value(..), object, toJSON)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import qualified Data.Map.Strict as Map
import UnliftIO.Exception (throwIO)
import Inngest.Types
import Inngest.Step
import Inngest.Function
import Inngest.Execution
import Inngest.Errors

emptyReq :: ServerRequest
emptyReq = ServerRequest
  { srCtx    = ServerCtx 0 False Nothing "run_1" []
  , srEvent  = mkEvent "app/created" (object [])
  , srEvents = [mkEvent "app/created" (object [])]
  , srSteps  = Map.empty
  , srUseApi = False
  }

fnOf :: (Ctx -> Event -> InngestT IO Value) -> Function IO
fnOf h = (createFunction (defaultFnOpts "f") [TriggerEvent "app/created" Nothing]
             (\_ _ -> pure (0 :: Int))) { fnHandler = h }

spec :: Spec
spec = describe "executeFunction" $ do
  it "returns 200 with the bare output on completion" $ do
    r <- executeFunction (fnOf (\_ _ -> pure (toJSON (42 :: Int)))) emptyReq Nothing
    erStatus r `shouldBe` 200
    erBody r   `shouldBe` toJSON (42 :: Int)

  it "returns 206 with an opcode array when a step interrupts" $ do
    r <- executeFunction (fnOf (\_ _ -> toJSON <$> stepRun "a" (pure (1 :: Int)))) emptyReq Nothing
    erStatus r `shouldBe` 206
    case erBody r of
      Array v -> do
        V.length v `shouldBe` 1
        case V.head v of
          Object o -> KM.lookup "op" o `shouldBe` Just (String "StepRun")
          _        -> expectationFailure "expected an opcode object"
      _ -> expectationFailure "expected a 206 array body"

  it "returns 500 with an error object on a function-level exception" $ do
    r <- executeFunction (fnOf (\_ _ -> throwIO (userError "boom"))) emptyReq Nothing
    erStatus r `shouldBe` 500
    erNoRetry r `shouldBe` False
    case erBody r of
      Object o -> do
        KM.member "code" o    `shouldBe` True
        KM.member "message" o `shouldBe` True
        KM.member "name" o    `shouldBe` True
      _ -> expectationFailure "expected an error object body"

  it "sets no-retry for a NonRetriableError" $ do
    r <- executeFunction (fnOf (\_ _ -> throwIO (NonRetriableError "nope"))) emptyReq Nothing
    erStatus r  `shouldBe` 500
    erNoRetry r `shouldBe` True
