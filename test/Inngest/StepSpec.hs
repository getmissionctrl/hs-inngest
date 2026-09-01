{-# LANGUAGE OverloadedStrings #-}
module Inngest.StepSpec (spec) where

import Test.Hspec
import Data.Aeson (Value(..), object, (.=), toJSON)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Inngest.Types
import Inngest.Step

-- A ServerRequest with the given memoized steps (keyed by hashed id).
reqWith :: [(Text, Value)] -> ServerRequest
reqWith steps = ServerRequest
  { srCtx    = ServerCtx 0 False Nothing "run_1" []
  , srEvent  = mkEvent "app/created" (object [])
  , srEvents = [mkEvent "app/created" (object [])]
  , srSteps  = Map.fromList steps
  , srUseApi = False
  }

-- Handler: two sequential steps, b depends on a.
twoStep :: InngestT IO Int
twoStep = do
  x <- stepRun "a" (pure (10 :: Int))
  stepRun "b" (pure (x + 1))

runH :: ServerRequest -> InngestT IO a -> IO (Either Interrupt a)
runH sr h = do
  st <- newExecState sr Nothing
  runInngestT st h

firstOp :: Either Interrupt a -> OutgoingOp
firstOp (Left (Interrupt (o:_))) = o
firstOp _ = error "expected an interrupt with at least one op"

spec :: Spec
spec = do
  describe "hashStepId" $
    it "is SHA1 hex of the id" $ do
      hashStepId "a"        `shouldBe` "86f7e437faa5a7fce15d1ddcb9eaeaea377667b8"
      hashStepId "step-one" `shouldBe` "536a1ebaf1da1fe4cde6b1e002b0230b80303167"

  describe "parseMemo" $ do
    it "unwraps a {data} output" $
      parseMemo (object ["data" .= (5 :: Int)]) `shouldBe` MemoData (toJSON (5 :: Int))
    it "reads an {error} output" $
      parseMemo (object ["error" .= object ["message" .= ("boom" :: String), "name" .= ("E" :: String)]])
        `shouldBe` MemoError (ErrorData "" "boom" "E" Nothing)
    it "treats a bare event object as data verbatim" $
      parseMemo (object ["name" .= ("evt" :: String), "data" .= object []])
        `shouldBe` MemoData (object ["name" .= ("evt" :: String), "data" .= object []])

  describe "replay loop" $ do
    it "interrupts at the first new step with its StepRun opcode" $ do
      r <- runH (reqWith []) twoStep
      let o = firstOp r
      siId (ooStep o)          `shouldBe` hashStepId "a"
      siOp (ooStep o)          `shouldBe` OpStepRun
      siDisplayName (ooStep o) `shouldBe` "a"
      ooData o                 `shouldBe` Just (toJSON (10 :: Int))

    it "returns the memoized result for a and interrupts at b" $ do
      r <- runH (reqWith [(hashStepId "a", object ["data" .= (10 :: Int)])]) twoStep
      let o = firstOp r
      siId (ooStep o) `shouldBe` hashStepId "b"
      ooData o        `shouldBe` Just (toJSON (11 :: Int))

    it "completes (Right) once every step is memoized" $ do
      r <- runH (reqWith [ (hashStepId "a", object ["data" .= (10 :: Int)])
                         , (hashStepId "b", object ["data" .= (99 :: Int)]) ]) twoStep
      r `shouldBe` Right 99

    it "dedups a reused id via the counter (second use hashes id:1)" $ do
      let h = do _ <- stepRun "a" (pure (1 :: Int)); stepRun "a" (pure (2 :: Int))
      r <- runH (reqWith [(hashStepId "a", object ["data" .= (1 :: Int)])]) h
      siId (ooStep (firstOp r)) `shouldBe` hashStepId "a:1"

  describe "sleep" $
    it "interrupts with a Sleep opcode carrying the until time in name" $ do
      r <- runH (reqWith []) (sleep "nap" (seconds 30))
      let o = firstOp r
      siOp (ooStep o) `shouldBe` OpSleep
      case siName (ooStep o) of
        Just t  -> t `shouldSatisfy` (\s -> not (null (show s)))
        Nothing -> expectationFailure "expected a sleep-until name"
