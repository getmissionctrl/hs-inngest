{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end example exercising every step tool against a locally-running
-- Inngest dev server:
--
--   * funcA — stepRun, sleep, parallel, waitForEvent, invoke
--   * funcB — the invoke target
--   * funcC — a terminal failure that triggers its onFailure handler
--
-- Serve the app, register it, then send events and assert both funcA's result
-- and that funcC's onFailure ran.
--
-- Run the dev server (see process-compose.yaml) then:
--   nix develop .#dev --command cabal run hs-inngest-example
module Main (main) where

import Control.Concurrent (forkIO, threadDelay, newEmptyMVar, putMVar, takeMVar, MVar)
import Control.Exception (throwIO)
import Control.Monad (void, when)
import Data.Aeson (object, (.=))
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Client (httpLbs, responseStatus, responseBody)
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Types.Status (statusCode)
import qualified Network.Wai.Handler.Warp as Warp
import System.Exit (exitFailure, exitSuccess)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import System.Timeout (timeout)

import Inngest

appPort :: Int
appPort = 8899

appUrl :: Text
appUrl = "http://127.0.0.1:8899/api/inngest"

cfg :: Config
cfg = (devConfig "hs-inngest-example") { cfgEventKey = Just "dev" }

-- | The invoke target: returns a fixed value.
funcB :: Function IO
funcB = createFunction (defaultFnOpts "funcB") [TriggerEvent "demo/never" Nothing] $ \_ _ ->
  pure (42 :: Int)

-- | The main pipeline: sequential steps, a real sleep, a parallel fan-out, a
-- wait for an approval event, and an invoke of funcB.
funcA :: MVar Int -> Function IO
funcA doneA =
  createFunction (defaultFnOpts "funcA") [TriggerEvent "demo/hello" Nothing] $ \_ _ -> do
    a        <- stepRun "one" (pure (1 :: Int))
    sleep "nap" (seconds 1)
    bs       <- parallel [ stepRun "p1" (pure (10 :: Int))
                         , stepRun "p2" (pure (20 :: Int)) ]
    approved <- waitForEvent "await-approve" (WaitOpts "demo/approve" (seconds 60) Nothing)
    inv      <- invoke "call-b" (FunctionRef (cfgAppId cfg) "funcB")
                       (object ["n" .= (21 :: Int)]) :: InngestT IO Int
    let total = a + sum bs + inv + (if isJust approved then 1000 else 0)
    stepRun "finish" $ do
      putMVar doneA total
      pure total

-- | A function that fails terminally, triggering its onFailure handler.
funcC :: MVar Bool -> Function IO
funcC doneFail =
  withOnFailure onFail $
    createFunction (defaultFnOpts "funcC") [TriggerEvent "demo/fail" Nothing] $ \_ _ -> do
      _ <- stepRun "explode" (throwIO (NonRetriableError "intentional failure") :: IO ())
      pure ()
  where
    onFail _ _ = stepRun "note-failure" $ do
      putMVar doneFail True
      pure True

post :: Text -> Text -> IO ()
post label body = putStrLn ("  " <> T.unpack label <> ": " <> T.unpack body)

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  doneA    <- newEmptyMVar
  doneFail <- newEmptyMVar
  let fns = [ funcA doneA, funcB, funcC doneFail ]

  putStrLn ("Serving app on " <> T.unpack appUrl)
  void $ forkIO $ Warp.run appPort (toApplication id cfg appUrl fns)
  threadDelay 500000

  mgr <- getGlobalManager

  putStrLn "Registering with the Inngest dev server..."
  regReq  <- buildRegisterRequest cfg appUrl (concatMap (functionConfigs (cfgAppId cfg) appUrl) fns) Nothing
  regResp <- httpLbs regReq mgr
  putStrLn ("  register -> HTTP " <> show (statusCode (responseStatus regResp)))
  when (statusCode (responseStatus regResp) >= 400) $ do
    putStrLn ("  register failed: " <> show (responseBody regResp)); exitFailure

  -- Kick off funcA and funcC.
  putStrLn "Sending events demo/hello and demo/fail..."
  _ <- send cfg [mkEvent "demo/hello" (object ["who" .= ("world" :: Text)])]
  _ <- send cfg [mkEvent "demo/fail"  (object [])]
  post "sent" "demo/hello, demo/fail"

  -- Deliver the approval funcA is waiting for (after it has reached the wait).
  void $ forkIO $ do
    threadDelay (6 * 1000000)
    _ <- send cfg [mkEvent "demo/approve" (object ["ok" .= True])]
    post "sent" "demo/approve (delayed)"

  putStrLn "Waiting for funcA result and funcC onFailure..."
  rA <- timeout (90 * 1000000) (takeMVar doneA)
  rF <- timeout (90 * 1000000) (takeMVar doneFail)

  let okA = rA == Just 1073   -- 1 + (10+20) + 42(invoke) + 1000(approved)
      okF = rF == Just True
  putStrLn ("  funcA result: " <> show rA <> (if okA then "  ✓" else "  ✗ (want Just 1073)"))
  putStrLn ("  funcC onFailure ran: " <> show rF <> (if okF then "  ✓" else "  ✗"))

  if okA && okF
    then putStrLn "E2E PASS: stepRun + sleep + parallel + waitForEvent + invoke + onFailure" >> exitSuccess
    else putStrLn "E2E FAIL" >> exitFailure
