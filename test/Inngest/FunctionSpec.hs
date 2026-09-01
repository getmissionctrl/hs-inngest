{-# LANGUAGE OverloadedStrings #-}
module Inngest.FunctionSpec (spec) where

import Test.Hspec
import Data.Aeson (Value, object, (.=))
import Inngest.Types
import Inngest.Step (Ctx, InngestT)
import Inngest.Function

handler :: Ctx -> Event -> InngestT IO Int
handler _ _ = pure 1

baseFn :: Function IO
baseFn = createFunction
  (defaultFnOpts "my-fn") { foRetries = Just 3, foName = Just "My Fn" }
  [TriggerEvent "app/created" Nothing]
  handler

spec :: Spec
spec = describe "functionConfigs" $ do
  it "serializes a single function config with retries in the step" $
    functionConfigs "myapp" "https://x/api/inngest" baseFn
      `shouldBe`
        [ object
          [ "id" .= ("myapp-my-fn" :: Value)
          , "name" .= ("My Fn" :: Value)
          , "triggers" .= [object ["event" .= ("app/created" :: Value)]]
          , "steps" .= object
              [ "step" .= object
                  [ "id" .= ("step" :: Value)
                  , "name" .= ("step" :: Value)
                  , "retries" .= object ["attempts" .= (3 :: Int)]
                  , "runtime" .= object
                      [ "type" .= ("http" :: Value)
                      , "url" .= ("https://x/api/inngest?fnId=myapp-my-fn&stepId=step" :: Value) ]
                  ]
              ]
          ]
        ]

  it "emits a second -failure config triggered by inngest/function.failed" $ do
    let cfgs = functionConfigs "myapp" "https://x/api/inngest" (withOnFailure handler baseFn)
    length cfgs `shouldBe` 2
    cfgs !! 1 `shouldBe`
      object
        [ "id" .= ("myapp-my-fn-failure" :: Value)
        , "name" .= ("My Fn (failure)" :: Value)
        , "triggers" .= [object
            [ "event" .= ("inngest/function.failed" :: Value)
            , "expression" .= ("event.data.function_id == 'myapp-my-fn'" :: Value) ]]
        , "steps" .= object
            [ "step" .= object
                [ "id" .= ("step" :: Value)
                , "name" .= ("step" :: Value)
                , "retries" .= object ["attempts" .= (0 :: Int)]
                , "runtime" .= object
                    [ "type" .= ("http" :: Value)
                    , "url" .= ("https://x/api/inngest?fnId=myapp-my-fn-failure&stepId=step" :: Value) ]
                ]
            ]
        ]
