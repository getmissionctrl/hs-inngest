{-# LANGUAGE OverloadedStrings #-}

-- | The step/function error taxonomy (spec §7.6). A step body (or handler) can
-- throw one of these to control retry behavior; the driver maps them to the
-- @x-inngest-no-retry@ / @retry-after@ headers and to StepError vs StepFailed.
module Inngest.Errors
  ( RetriableError(..)
  , NonRetriableError(..)
  , RetryAfterError(..)
  , isRetriableException
  ) where

import Control.Exception (Exception, SomeException, fromException)
import Data.Text (Text)
import Data.Time (UTCTime)

-- | An explicitly retriable failure (the default behavior for any exception).
newtype RetriableError = RetriableError Text
  deriving (Show)
instance Exception RetriableError

-- | A terminal failure: the server must not retry (@x-inngest-no-retry: true@).
newtype NonRetriableError = NonRetriableError Text
  deriving (Show)
instance Exception NonRetriableError

-- | Retry, but not before the given time (@retry-after@).
data RetryAfterError = RetryAfterError Text UTCTime
  deriving (Show)
instance Exception RetryAfterError

-- | Whether an exception should be retried. Only 'NonRetriableError' is
-- non-retriable; everything else (including 'RetryAfterError') is retriable.
isRetriableException :: SomeException -> Bool
isRetriableException e = case fromException e of
  Just (NonRetriableError _) -> False
  _                          -> True
