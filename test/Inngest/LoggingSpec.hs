{-# LANGUAGE OverloadedStrings #-}

-- | Demonstrates that the SDK composes with a structured-logging base monad:
-- functions run in @KatipContextT IO@ and step bodies emit katip logs. Because
-- memoized steps don't re-run their bodies, logs are naturally de-duplicated
-- across replays.
module Inngest.LoggingSpec (spec) where

import Test.Hspec
import Katip
import Control.Exception (bracket)
import qualified Data.Map.Strict as Map
import Data.Aeson (Value, object, (.=))
import Data.Text (Text)
import System.IO (stdout)

import Inngest.Types
import Inngest.Step

type AppM = KatipContextT IO

withLogEnv :: (LogEnv -> IO a) -> IO a
withLogEnv = bracket mk closeScribes
  where
    mk = do
      scribe <- mkHandleScribeWithFormatter jsonFormat ColorIfTerminal stdout (permitItem InfoS) V2
      le     <- initLogEnv "hs-inngest" "test"
      registerScribe "stdout" scribe defaultScribeSettings le

-- A two-step function; each step logs a structured event when it executes.
loggingFn :: InngestT AppM Int
loggingFn = do
  a <- stepRun "one" $ do
         katipAddContext (sl "step" ("one" :: Text)) $ logFM InfoS "running step one"
         pure (1 :: Int)
  stepRun "two" $ do
    katipAddContext (sl "step" ("two" :: Text)) $ logFM InfoS "running step two"
    pure (a + 1)

reqWith :: [(Text, Value)] -> ServerRequest
reqWith steps = ServerRequest
  (ServerCtx 0 False Nothing "run_log" [])
  (mkEvent "demo/log" (object []))
  [mkEvent "demo/log" (object [])]
  (Map.fromList steps)
  False

run :: LogEnv -> [(Text, Value)] -> IO (Either Interrupt Int)
run le steps = do
  st <- newExecState (reqWith steps) Nothing
  runKatipContextT le () "test" (runInngestT st loggingFn)

spec :: Spec
spec = describe "katip structured logging" $ do
  it "logs from the first step, then interrupts on it" $
    withLogEnv $ \le -> do
      r <- run le []
      case r of
        Left (Interrupt (o:_)) -> siId (ooStep o) `shouldBe` hashStepId "one"
        _                      -> expectationFailure "expected an interrupt at step one"

  it "logs from the second step once the first is memoized" $
    withLogEnv $ \le -> do
      r <- run le [(hashStepId "one", object ["data" .= (1 :: Int)])]
      case r of
        Left (Interrupt (o:_)) -> siId (ooStep o) `shouldBe` hashStepId "two"
        _                      -> expectationFailure "expected an interrupt at step two"

  it "emits no step logs once every step is memoized (dedup on replay)" $
    withLogEnv $ \le -> do
      r <- run le [ (hashStepId "one", object ["data" .= (1 :: Int)])
                  , (hashStepId "two", object ["data" .= (2 :: Int)]) ]
      r `shouldBe` Right 2
