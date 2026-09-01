{-# LANGUAGE OverloadedStrings #-}
module Inngest.TypesSpec (spec) where

import Test.Hspec
import Data.Aeson (toJSON, object, (.=), Value(..), eitherDecode)
import Data.Time (UTCTime(..), fromGregorian, secondsToDiffTime, picosecondsToDiffTime)
import Inngest.Types

spec :: Spec
spec = do
  describe "Event JSON" $ do
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

  describe "Opcode strings" $
    it "match the Inngest enum" $ do
      opcodeText OpStepRun        `shouldBe` "StepRun"
      opcodeText OpSleep          `shouldBe` "Sleep"
      opcodeText OpWaitForEvent   `shouldBe` "WaitForEvent"
      opcodeText OpInvokeFunction `shouldBe` "InvokeFunction"
      opcodeText OpStepPlanned    `shouldBe` "StepPlanned"
      opcodeText OpStepError      `shouldBe` "StepError"
      opcodeText OpStepFailed     `shouldBe` "StepFailed"

  describe "Opcode encoding" $ do
    it "encodes a StepRun success with data and omits null opts" $
      encodeOp (OutgoingOp (stepInfo "h1" OpStepRun "my step")
                           (Just (object ["v" .= (42 :: Int)])) Nothing)
        `shouldBe` object [ "id" .= ("h1" :: String)
                          , "op" .= ("StepRun" :: String)
                          , "displayName" .= ("my step" :: String)
                          , "name" .= Null
                          , "data" .= object ["v" .= (42 :: Int)] ]

    it "encodes a Sleep with the until time in name" $
      encodeOp (OutgoingOp (stepInfo "h2" OpSleep "nap")
                             { siName = Just "2026-09-01T00:00:01.000Z" } Nothing Nothing)
        `shouldBe` object [ "id" .= ("h2" :: String)
                          , "op" .= ("Sleep" :: String)
                          , "displayName" .= ("nap" :: String)
                          , "name" .= ("2026-09-01T00:00:01.000Z" :: String) ]

    it "encodes a StepError with an error object" $
      encodeOp (OutgoingOp (stepInfo "h3" OpStepError "boom") Nothing
                           (Just (ErrorData "err" "kaboom" "MyError" Nothing)))
        `shouldBe` object [ "id" .= ("h3" :: String)
                          , "op" .= ("StepError" :: String)
                          , "displayName" .= ("boom" :: String)
                          , "name" .= Null
                          , "error" .= object [ "code" .= ("err" :: String)
                                              , "message" .= ("kaboom" :: String)
                                              , "name" .= ("MyError" :: String)
                                              , "stack" .= Null ] ]

  describe "Duration" $ do
    it "picks the largest evenly-dividing unit" $ do
      toDurationStr (seconds 30) `shouldBe` Right "30s"
      toDurationStr (hours 1)    `shouldBe` Right "1h"
      toDurationStr (days 3)     `shouldBe` Right "3d"
      toDurationStr (days 7)     `shouldBe` Right "1w"  -- 7d divides evenly into 1w
      toDurationStr (weeks 1)    `shouldBe` Right "1w"
      toDurationStr (seconds 90) `shouldBe` Right "90s"
    it "rejects sub-second durations" $
      toDurationStr (Duration 500) `shouldBe` Left "duration must be at least 1 second"

  describe "toIsoUtc" $
    it "renders millisecond precision with a Z suffix" $
      toIsoUtc (UTCTime (fromGregorian 2026 9 1)
                        (secondsToDiffTime 1 + picosecondsToDiffTime 789000000000))
        `shouldBe` "2026-09-01T00:00:01.789Z"

  describe "Trigger JSON" $ do
    it "encodes an event trigger" $
      toJSON (TriggerEvent "app/created" Nothing)
        `shouldBe` object ["event" .= ("app/created" :: String)]
    it "encodes a cron trigger" $
      toJSON (TriggerCron "0 * * * *" Nothing)
        `shouldBe` object ["cron" .= ("0 * * * *" :: String)]

  describe "ServerRequest decode" $
    it "reads ctx (attempt, run_id, nested stack, disable flag) and steps" $ do
      let raw = "{\"ctx\":{\"attempt\":2,\"run_id\":\"01H\",\"disable_immediate_execution\":true,\
                \\"stack\":{\"stack\":[\"a\",\"b\"]}},\
                \\"event\":{\"name\":\"app/created\",\"data\":{}},\
                \\"steps\":{\"h1\":{\"data\":1}},\"use_api\":false}"
      case eitherDecode raw :: Either String ServerRequest of
        Left e   -> expectationFailure e
        Right sr -> do
          scAttempt (srCtx sr) `shouldBe` 2
          scRunId (srCtx sr)   `shouldBe` "01H"
          scDisableImmediateExecution (srCtx sr) `shouldBe` True
          scStack (srCtx sr)   `shouldBe` ["a", "b"]
          eventName (srEvent sr) `shouldBe` "app/created"
