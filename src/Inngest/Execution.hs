{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | The replay driver: run a function handler against an incoming request and
-- reduce the outcome to an HTTP-shaped result — 200 completion, 206 opcode
-- array, or 500 function error — plus the conditional retry headers (spec §4).
module Inngest.Execution
  ( ExecResult(..)
  , executeFunction
  , renderBody
  ) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Time (UTCTime)
import UnliftIO.Exception (try, SomeException, displayException, fromException)

import Inngest.Types
import Inngest.Step
import Inngest.Function
import Inngest.Errors

-- | The core of an HTTP response, before the standard header block / signature
-- are layered on by the serve adapter.
data ExecResult = ExecResult
  { erStatus     :: Int
  , erBody       :: Value           -- ^ 200: bare output; 206: opcode array; 500: error object
  , erNoRetry    :: Bool            -- ^ emit @x-inngest-no-retry: true@
  , erRetryAfter :: Maybe UTCTime   -- ^ emit @retry-after@
  } deriving (Show)

-- | Serialize the result body to bytes (what gets signed and sent).
renderBody :: ExecResult -> BL.ByteString
renderBody = encode . erBody

-- | Run a function for one request. @target@ is the hashed step id to target
-- under stepId routing (Nothing for the root "step").
executeFunction
  :: MonadInngest m
  => Function m
  -> ServerRequest
  -> Maybe HashedStepId
  -> m ExecResult
executeFunction fn sr target = do
  st  <- liftIO (newExecState sr target)
  let ctx = mkCtx sr
      ev  = srEvent sr
  outcome <- try (runInngestT st (fnHandler fn ctx ev))
  pure $ case outcome of
    Right (Right out) ->
      ExecResult 200 out False Nothing
    Right (Left (Interrupt ops)) ->
      ExecResult 206 (toJSON (map encodeOp ops)) (any isTerminalOp ops) (retryAfterOfOps ops)
    Right (Left SkipStep) ->
      -- A skip that reached the top: no targeted step matched. Empty completion.
      ExecResult 200 Null False Nothing
    Left (e :: SomeException) ->
      let ed = errorDataOf e
      in ExecResult 500 (toJSON ed) (functionNoRetry e) (retryAfterOfException e)
  where
    isTerminalOp o = siOp (ooStep o) == OpStepFailed

-- A rethrown terminal StepFailure is non-retriable; otherwise defer to the type.
functionNoRetry :: SomeException -> Bool
functionNoRetry e = case fromException e of
  Just (StepFailure _) -> True
  _                    -> not (isRetriableException e)

-- Determine the ErrorData for a function-level exception, preserving a rethrown
-- memoized StepFailure and mapping known error types.
errorDataOf :: SomeException -> ErrorData
errorDataOf e
  | Just (StepFailure ed) <- fromException e = ed
  | Just (NonRetriableError msg) <- fromException e =
      ErrorData "non_retriable_error" msg "NonRetriableError" Nothing
  | Just (RetryAfterError msg _) <- fromException e =
      ErrorData "retry_after_error" msg "RetryAfterError" Nothing
  | otherwise = ErrorData "unknown" (T.pack (displayException e)) "Error" Nothing

retryAfterOfException :: SomeException -> Maybe UTCTime
retryAfterOfException e = case fromException e of
  Just (RetryAfterError _ t) -> Just t
  _                          -> Nothing

retryAfterOfOps :: [OutgoingOp] -> Maybe UTCTime
retryAfterOfOps _ = Nothing
