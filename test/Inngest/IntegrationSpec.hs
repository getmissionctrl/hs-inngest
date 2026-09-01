{-# LANGUAGE OverloadedStrings #-}

-- | End-to-end: drive a multi-step + sleep + parallel function to completion by
-- simulating the Inngest server's stateless-replay loop against the real serve
-- layer (no network / dev-server binary required). Each 206 opcode is
-- "fulfilled" and fed back as a memoized step until the function returns 200.
module Inngest.IntegrationSpec (spec) where

import Test.Hspec hiding (parallel)
import Data.Aeson (Value(..), object, (.=), decode, toJSON, encode)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Vector as V
import Inngest.Config
import Inngest.Types (Trigger(..), seconds)
import Inngest.Step
import Inngest.Function
import Inngest.Serve.Servant

cfg :: Config
cfg = devConfig "app"

-- A pipeline exercising sequential steps, a sleep, and a parallel fan-out.
pipeline :: Function IO
pipeline = createFunction (defaultFnOpts "pipeline") [TriggerEvent "app/created" Nothing] $ \_ _ -> do
  a  <- stepRun "one" (pure (1 :: Int))
  sleep "wait" (seconds 30)
  bs <- parallel [ stepRun "p1" (pure (10 :: Int))
                 , stepRun "p2" (pure (20 :: Int)) ]
  stepRun "two" (pure (a + sum bs))

serve :: [(Text, Maybe Text)] -> Map Text Value -> IO ServeOutput
serve q steps =
  handleInngest cfg "https://x/api/inngest" [pipeline]
    (ServeInput MPOST (("fnId", Just "app-pipeline") : q) [] (body steps))
  where
    body s = encodeBody $ object
      [ "ctx"   .= object [ "run_id" .= ("r" :: Text), "attempt" .= (0 :: Int)
                          , "stack" .= object ["stack" .= ([] :: [Text])] ]
      , "event" .= object ["name" .= ("app/created" :: Text), "data" .= object []]
      , "steps" .= Object (KM.fromList [ (K.fromText k, v) | (k, v) <- Map.toList s ])
      , "use_api" .= False
      ]
    encodeBody = encode

-- Record every StepRun opcode (id -> {data}); return whether any Sleep/Planned needs follow-up.
opsOf :: ServeOutput -> [Value]
opsOf out = case decode (soBody out) :: Maybe Value of
  Just (Array v) -> V.toList v
  _              -> []

opField :: Text -> Value -> Maybe Value
opField k (Object o) = KM.lookup (K.fromText k) o
opField _ _          = Nothing

opText :: Text -> Value -> Maybe Text
opText k v = case opField k v of Just (String s) -> Just s; _ -> Nothing

-- Simulate the server: fulfill opcodes, feeding memos back until 200.
drive :: IO Value
drive = go Map.empty (0 :: Int)
  where
    go _ 30 = expectationFailure' "replay did not converge"
    go steps n = do
      out <- serve [] steps
      if soStatus out == 200
        then pure (maybe Null id (decode (soBody out)))
        else do
          steps' <- foldFulfil steps (opsOf out)
          go steps' (n + 1)

    -- Fulfill each opcode into the steps map (targeting planned steps as needed).
    foldFulfil steps []       = pure steps
    foldFulfil steps (op:ops) = do
      steps2 <- fulfil steps op
      foldFulfil steps2 ops

    fulfil steps op = case (opText "op" op, opText "id" op) of
      (Just "StepRun", Just h)      -> pure (Map.insert h (wrap (opField "data" op)) steps)
      (Just "Sleep", Just h)        -> pure (Map.insert h Null steps)
      (Just "StepPlanned", Just h)  -> do
        -- targeted re-invocation actually runs the planned step
        out <- serve [("stepId", Just h)] steps
        case [ (hid, opField "data" o) | o <- opsOf out
                                       , opText "op" o == Just "StepRun"
                                       , Just hid <- [opText "id" o] ] of
          ((hid, d):_) -> pure (Map.insert hid (wrap d) steps)
          []           -> pure steps
      _ -> pure steps

    wrap (Just d) = object ["data" .= d]
    wrap Nothing  = Null

expectationFailure' :: String -> IO a
expectationFailure' m = expectationFailure m >> error m

spec :: Spec
spec = describe "end-to-end replay" $
  it "drives a step + sleep + parallel pipeline to completion" $ do
    result <- drive
    result `shouldBe` toJSON (31 :: Int)   -- 1 + (10 + 20)
