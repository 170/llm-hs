{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLM.Ollama (callOllama) where

import Control.Exception (try, SomeException)
import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), withObject, decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit ((.|), runConduit, ConduitT)
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import Network.HTTP.Simple
import Network.HTTP.Conduit (responseBody)
import qualified LLM.Types as Types
import LLM.Types (LLMRequest, LLMResponse(..), LLMError(..))
import LLM.Spinner (stopSpinner)

data OllamaRequest = OllamaRequest
  { requestModel :: Text
  , ollamaPrompt :: Text
  , requestStream :: Bool
  } deriving (Show)

instance ToJSON OllamaRequest where
  toJSON (OllamaRequest m p s) =
    object ["model" .= m, "prompt" .= p, "stream" .= s]

data OllamaResponse = OllamaResponse
  { response :: Text
  } deriving (Show)

instance FromJSON OllamaResponse where
  parseJSON = withObject "OllamaResponse" $ \v ->
    OllamaResponse <$> v .: "response"

callOllama :: LLMRequest -> IO (Either LLMError LLMResponse)
callOllama llmReq = do
  let model' = case Types.model llmReq of
        Nothing -> "llama3.2"
        Just m -> m
      baseUrl' = case Types.baseUrl llmReq of
        Nothing -> "localhost:11434"
        Just u -> u
      -- Build prompt with conversation history
      historyText = T.intercalate "\n" $ map (\msg ->
        Types.role msg <> ": " <> Types.messageContent msg) (Types.history llmReq)
      fullPrompt = if T.null historyText
                   then Types.prompt llmReq
                   else historyText <> "\nuser: " <> Types.prompt llmReq

  if Types.streaming llmReq
    then callOllamaStream baseUrl' model' fullPrompt
    else callOllamaNonStream baseUrl' model' fullPrompt

callOllamaNonStream :: Text -> Text -> Text -> IO (Either LLMError LLMResponse)
callOllamaNonStream baseUrl' model' prompt' = do
  result <- try $ do
    let requestBody = OllamaRequest
          { requestModel = model'
          , ollamaPrompt = prompt'
          , requestStream = False
          }

    request' <- parseRequest $ "POST http://" <> T.unpack baseUrl' <> "/api/generate"
    let request = setRequestBodyJSON requestBody request'

    response <- httpJSON request
    return $ getResponseBody response :: IO OllamaResponse

  case result of
    Left (e :: SomeException) -> return $ Left $ NetworkError (show e)
    Right ollamaResp -> return $ Right $ LLMResponse (response ollamaResp)

callOllamaStream :: Text -> Text -> Text -> IO (Either LLMError LLMResponse)
callOllamaStream baseUrl' model' prompt' = do
  result <- try $ do
    let requestBody = OllamaRequest
          { requestModel = model'
          , ollamaPrompt = prompt'
          , requestStream = True
          }

    request' <- parseRequest $ "POST http://" <> T.unpack baseUrl' <> "/api/generate"
    let request = setRequestBodyJSON requestBody request'

    withResponse request $ \resp -> do
      firstChunkRef <- newIORef True
      runConduit $ responseBody resp .| CC.linesUnboundedAscii .| CL.mapM_ (processOllamaChunk firstChunkRef)
      return ()

  case result of
    Left (e :: SomeException) -> return $ Left $ NetworkError (show e)
    Right () -> return $ Right $ LLMResponse ""

processOllamaChunk :: IORef Bool -> BS.ByteString -> IO ()
processOllamaChunk firstChunkRef chunk
  | BS.null chunk = return ()
  | otherwise = case decode (LBS.fromStrict chunk) of
      Just (ollamaResp :: OllamaResponse) -> do
        let txt = response ollamaResp
        -- Stop spinner on first chunk with content
        when (not $ T.null txt) $ do
          isFirst <- readIORef firstChunkRef
          when isFirst $ do
            writeIORef firstChunkRef False
            stopSpinner
        -- Output the content immediately
        TIO.putStr txt
        hFlush stdout
      Nothing -> return ()
