{-# LANGUAGE OverloadedStrings #-}

module Core.Retry
  ( retryWithBackoff
  , defaultRetryPolicy
  , isRetriableException
  ) where

import Control.Exception (SomeException, fromException)
import Control.Monad.IO.Class (MonadIO)
import Control.Retry
import Network.HTTP.Client (HttpException(..), HttpExceptionContent(..))
import qualified Data.Text as T
import qualified Core.Types as Types

-- | Default retry policy: exponential backoff starting at 100ms, max 3 retries
defaultRetryPolicy :: RetryPolicyM IO
defaultRetryPolicy = exponentialBackoff 100000 <> limitRetries 3

-- | Check if an exception is retriable
isRetriableException :: SomeException -> Bool
isRetriableException e = case fromException e of
  Just (HttpExceptionRequest _ content) -> isRetriableHttpContent content
  Just (InvalidUrlException _ _) -> False
  Nothing -> False
  where
    isRetriableHttpContent :: HttpExceptionContent -> Bool
    isRetriableHttpContent ResponseTimeout = True
    isRetriableHttpContent ConnectionTimeout = True
    isRetriableHttpContent (StatusCodeException _ _) = False
    isRetriableHttpContent ConnectionFailure {} = True
    isRetriableHttpContent _ = False

-- | Retry an IO action with exponential backoff for retriable errors
retryWithBackoff :: MonadIO m
                 => RetryPolicyM m
                 -> (RetryStatus -> m (Either Types.LLMError a))
                 -> m (Either Types.LLMError a)
retryWithBackoff policy = retrying policy shouldRetry
  where
    shouldRetry :: Monad m => RetryStatus -> Either Types.LLMError a -> m Bool
    shouldRetry _ (Right _) = return False
    shouldRetry _ (Left err) = return $ isRetriableError err

    isRetriableError :: Types.LLMError -> Bool
    isRetriableError (Types.NetworkError msg) =
      let msg' = T.toLower msg
      in T.isInfixOf "responsetimeout" msg'
         || T.isInfixOf "connectiontimeout" msg'
         || T.isInfixOf "timeout" msg'
    isRetriableError _ = False
