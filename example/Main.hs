{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end example with structured logging (katip).
--
-- Functions run in @KatipContextT IO@ — a 'MonadUnliftIO', hence a valid
-- 'MonadInngest' base monad — so step bodies emit structured logs. Because
-- memoized steps don't re-run, those logs are naturally de-duplicated across
-- the replay loop. Exercises every step tool against a real Inngest dev server:
-- stepRun, sleep, parallel, waitForEvent, invoke, and onFailure.
--
--   nix develop .#dev --command cabal run hs-inngest-example
module Main (main) where

import Control.Concurrent (forkIO, threadDelay, newEmptyMVar, putMVar, takeMVar, MVar)
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (object, (.=))
import Data.Maybe (isJust)
import Data.Text (Text)
import Katip
import Network.HTTP.Client (httpLbs, responseStatus, responseBody)
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.HTTP.Types.Status (statusCode)
import qualified Network.Wai.Handler.Warp as Warp
import System.Exit (exitFailure, exitSuccess)
import System.IO (hSetBuffering, stdout, BufferMode(LineBuffering))
import System.Timeout (timeout)
import UnliftIO.Exception (bracket, throwIO)

import Inngest

-- | The app's base monad: IO plus katip structured-logging context.
type AppM = KatipContextT IO

appPort :: Int
appPort = 8899

appUrl :: Text
appUrl = "http://127.0.0.1:8899/api/inngest"

cfg :: Config
cfg = (devConfig "hs-inngest-example") { cfgEventKey = Just "dev" }

-- | Log a structured event tagged with the step name (inside a step body).
logStep :: KatipContext m => Text -> LogStr -> m ()
logStep name msg = katipAddContext (sl "step" name) $ logFM InfoS msg

-- | The invoke target.
funcB :: Function AppM
funcB = createFunction (defaultFnOpts "funcB") [TriggerEvent "demo/never" Nothing] $ \_ _ ->
  stepRun "b-work" $ do
    logStep "funcB" "invoked"
    pure (42 :: Int)

-- | The main pipeline.
funcA :: MVar Int -> Function AppM
funcA doneA =
  createFunction (defaultFnOpts "funcA") [TriggerEvent "demo/hello" Nothing] $ \_ _ -> do
    a        <- stepRun "one" (logStep "one" "computing base" >> pure (1 :: Int))
    sleep "nap" (seconds 1)
    bs       <- parallel [ stepRun "p1" (logStep "p1" "fan-out branch" >> pure (10 :: Int))
                         , stepRun "p2" (logStep "p2" "fan-out branch" >> pure (20 :: Int)) ]
    approved <- waitForEvent "await-approve" (WaitOpts "demo/approve" (seconds 60) Nothing)
    inv      <- invoke "call-b" (FunctionRef (cfgAppId cfg) "funcB")
                       (object ["n" .= (21 :: Int)]) :: InngestT AppM Int
    let total = a + sum bs + inv + (if isJust approved then 1000 else 0)
    stepRun "finish" $ do
      katipAddContext (sl "total" total <> sl "approved" (isJust approved) <> sl "invoke" inv) $
        logFM InfoS "pipeline complete"
      liftIO (putMVar doneA total)
      pure total

-- | A function that fails terminally, triggering its onFailure handler.
funcC :: MVar Bool -> Function AppM
funcC doneFail =
  withOnFailure onFail $
    createFunction (defaultFnOpts "funcC") [TriggerEvent "demo/fail" Nothing] $ \_ _ ->
      stepRun "explode" (logStep "funcC" "about to fail"
                          >> throwIO (NonRetriableError "intentional failure")) :: InngestT AppM ()
  where
    onFail _ _ = stepRun "note-failure" $ do
      logStep "funcC" "onFailure ran"
      liftIO (putMVar doneFail True)
      pure True

makeLogEnv :: IO LogEnv
makeLogEnv = do
  scribe <- mkHandleScribeWithFormatter jsonFormat ColorIfTerminal stdout (permitItem InfoS) V2
  le     <- initLogEnv "hs-inngest-example" "development"
  registerScribe "stdout" scribe defaultScribeSettings le

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  doneA    <- newEmptyMVar
  doneFail <- newEmptyMVar
  let fns = [ funcA doneA, funcB, funcC doneFail ]

  bracket makeLogEnv closeScribes $ \le -> do
    -- Serve, running each request in the katip context.
    void $ forkIO $ Warp.run appPort (toApplication (runKatipContextT le () "inngest") cfg appUrl fns)
    threadDelay 500000
    mgr <- getGlobalManager

    runKatipContextT le () "driver" $ do
      logFM InfoS (ls ("serving app on " <> appUrl))

      regReq  <- liftIO $ buildRegisterRequest cfg appUrl (concatMap (functionConfigs (cfgAppId cfg) appUrl) fns) Nothing
      regResp <- liftIO $ httpLbs regReq mgr
      katipAddContext (sl "status" (statusCode (responseStatus regResp))) $
        logFM InfoS "registered app with dev server"
      when (statusCode (responseStatus regResp) >= 400) $ liftIO $ do
        putStrLn ("register failed: " <> show (responseBody regResp)); exitFailure

      _ <- liftIO $ send cfg [mkEvent "demo/hello" (object ["who" .= ("world" :: Text)])]
      _ <- liftIO $ send cfg [mkEvent "demo/fail"  (object [])]
      logFM InfoS "sent demo/hello and demo/fail"

      void $ liftIO $ forkIO $ do
        threadDelay (6 * 1000000)
        void $ send cfg [mkEvent "demo/approve" (object ["ok" .= True])]

      logFM InfoS "waiting for funcA result and funcC onFailure"
      rA <- liftIO $ timeout (90 * 1000000) (takeMVar doneA)
      rF <- liftIO $ timeout (90 * 1000000) (takeMVar doneFail)
      katipAddContext (sl "funcA" (show rA) <> sl "onFailure" (show rF)) $
        logFM InfoS "results collected"

      let okA = rA == Just 1073   -- 1 + (10+20) + 42 + 1000
          okF = rF == Just True
      liftIO $ if okA && okF
        then putStrLn "E2E PASS: stepRun + sleep + parallel + waitForEvent + invoke + onFailure" >> exitSuccess
        else putStrLn ("E2E FAIL: funcA=" <> show rA <> " onFailure=" <> show rF) >> exitFailure
