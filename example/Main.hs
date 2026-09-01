{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end example: serve an Inngest app, register it with a locally-running
-- Inngest dev server, send an event, and wait for a durable
-- step + sleep + parallel pipeline to run to completion.
--
-- Run the dev server (see process-compose.yaml) then:
--   nix develop .#dev --command cabal run hs-inngest-example
module Main (main) where

import Control.Concurrent (forkIO, threadDelay, newEmptyMVar, putMVar, takeMVar, MVar)
import Control.Monad (void, when)
import Data.Aeson (object, (.=))
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

-- | A durable pipeline: sequential steps, a real sleep, and a parallel fan-out.
-- The final step signals completion via the shared MVar.
pipeline :: MVar Int -> Function IO
pipeline done =
  createFunction (defaultFnOpts "pipeline") [TriggerEvent "demo/hello" Nothing] $ \_ _ -> do
    a  <- stepRun "one" (pure (1 :: Int))
    sleep "nap" (seconds 1)
    bs <- parallel [ stepRun "p1" (pure (10 :: Int))
                   , stepRun "p2" (pure (20 :: Int)) ]
    let total = a + sum bs
    stepRun "finish" $ do
      putMVar done total
      pure total

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  done <- newEmptyMVar
  let fn  = pipeline done
      fns = [fn]

  putStrLn ("Serving app on " <> T.unpack appUrl)
  void $ forkIO $ Warp.run appPort (toApplication id cfg appUrl fns)
  threadDelay 500000  -- let warp bind the socket

  mgr <- getGlobalManager

  -- Register the app with the dev server (out-of-band POST /fn/register).
  putStrLn "Registering with the Inngest dev server..."
  regReq <- buildRegisterRequest cfg appUrl (concatMap (functionConfigs (cfgAppId cfg) appUrl) fns) Nothing
  regResp <- httpLbs regReq mgr
  putStrLn ("  register -> HTTP " <> show (statusCode (responseStatus regResp)))
  when (statusCode (responseStatus regResp) >= 400) $ do
    putStrLn ("  register failed: " <> show (responseBody regResp))
    exitFailure

  -- Send the triggering event.
  putStrLn "Sending event demo/hello..."
  sent <- send cfg [mkEvent "demo/hello" (object ["who" .= ("world" :: Text)])]
  case sent of
    Left e    -> putStrLn ("  send failed: " <> e) >> exitFailure
    Right ids -> putStrLn ("  sent, event ids: " <> show ids)

  -- Wait for the function to run to completion (dev server drives the replay loop).
  putStrLn "Waiting for the pipeline to complete..."
  result <- timeout (60 * 1000000) (takeMVar done)
  case result of
    Just n
      | n == 31   -> putStrLn ("E2E PASS: pipeline returned " <> show n) >> exitSuccess
      | otherwise -> putStrLn ("E2E FAIL: expected 31, got " <> show n) >> exitFailure
    Nothing -> putStrLn "E2E FAIL: timed out waiting for completion" >> exitFailure
