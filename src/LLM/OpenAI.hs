{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLM.OpenAI (callOpenAI) where

import Control.Exception (try, SomeException)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?), withObject, decode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit ((.|), runConduit, ConduitT, await, yield)
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout, stderr)
import Network.HTTP.Simple
import Network.HTTP.Conduit (responseBody)
import qualified LLM.Types as Types
import LLM.Types (LLMRequest, LLMResponse(..), LLMError(..))
import LLM.Spinner (stopSpinner)

data OpenAIMessage = OpenAIMessage
  { role :: Text
  , messageContent :: Text
  } deriving (Show)

instance ToJSON OpenAIMessage where
  toJSON (OpenAIMessage r c) = object ["role" .= r, "content" .= c]

instance FromJSON OpenAIMessage where
  parseJSON = withObject "OpenAIMessage" $ \v ->
    OpenAIMessage <$> v .: "role" <*> v .: "content"

data OpenAIRequest = OpenAIRequest
  { requestModel :: Text
  , messages :: [OpenAIMessage]
  , requestStream :: Bool
  } deriving (Show)

instance ToJSON OpenAIRequest where
  toJSON (OpenAIRequest m msgs s) =
    object ["model" .= m, "messages" .= msgs, "stream" .= s]

data OpenAIChoice = OpenAIChoice
  { message :: OpenAIMessage
  } deriving (Show)

instance FromJSON OpenAIChoice where
  parseJSON = withObject "OpenAIChoice" $ \v ->
    OpenAIChoice <$> v .: "message"

data OpenAIResponse = OpenAIResponse
  { choices :: [OpenAIChoice]
  } deriving (Show)

instance FromJSON OpenAIResponse where
  parseJSON = withObject "OpenAIResponse" $ \v ->
    OpenAIResponse <$> v .: "choices"

-- Streaming types
data OpenAIDelta = OpenAIDelta
  { deltaContent :: Maybe Text
  } deriving (Show)

instance FromJSON OpenAIDelta where
  parseJSON = withObject "OpenAIDelta" $ \v ->
    OpenAIDelta <$> v .:? "content"

data OpenAIStreamChoice = OpenAIStreamChoice
  { delta :: OpenAIDelta
  } deriving (Show)

instance FromJSON OpenAIStreamChoice where
  parseJSON = withObject "OpenAIStreamChoice" $ \v ->
    OpenAIStreamChoice <$> v .: "delta"

data OpenAIStreamResponse = OpenAIStreamResponse
  { streamChoices :: [OpenAIStreamChoice]
  } deriving (Show)

instance FromJSON OpenAIStreamResponse where
  parseJSON = withObject "OpenAIStreamResponse" $ \v ->
    OpenAIStreamResponse <$> v .: "choices"

callOpenAI :: LLMRequest -> IO (Either LLMError LLMResponse)
callOpenAI llmReq = do
  let apiKey' = case Types.apiKey llmReq of
        Nothing -> error "OpenAI API key is required"
        Just k -> k
      model' = case Types.model llmReq of
        Nothing -> "gpt-4o-mini"
        Just m -> m
      -- Build messages from history + current prompt
      historyMessages = map (\msg -> OpenAIMessage (Types.role msg) (Types.messageContent msg)) (Types.history llmReq)
      allMessages = historyMessages ++ [OpenAIMessage "user" (Types.prompt llmReq)]

  if Types.streaming llmReq
    then callOpenAIStream apiKey' model' allMessages
    else callOpenAINonStream apiKey' model' allMessages

callOpenAINonStream :: Text -> Text -> [OpenAIMessage] -> IO (Either LLMError LLMResponse)
callOpenAINonStream apiKey' model' messages' = do
  result <- try $ do
    let requestBody = OpenAIRequest
          { requestModel = model'
          , messages = messages'
          , requestStream = False
          }

    request' <- parseRequest "POST https://api.openai.com/v1/chat/completions"
    let request = setRequestBodyJSON requestBody
                $ setRequestHeader "Authorization" ["Bearer " <> TE.encodeUtf8 apiKey']
                $ request'

    response <- httpJSON request
    return $ getResponseBody response :: IO OpenAIResponse

  case result of
    Left (e :: SomeException) -> return $ Left $ NetworkError (show e)
    Right openAIResp ->
      case choices openAIResp of
        [] -> return $ Left $ APIError "No response from OpenAI"
        (choice:_) -> return $ Right $ LLMResponse (messageContent $ message choice)

callOpenAIStream :: Text -> Text -> [OpenAIMessage] -> IO (Either LLMError LLMResponse)
callOpenAIStream apiKey' model' messages' = do
  result <- try $ do
    let requestBody = OpenAIRequest
          { requestModel = model'
          , messages = messages'
          , requestStream = True
          }

    request' <- parseRequest "POST https://api.openai.com/v1/chat/completions"
    let request = setRequestBodyJSON requestBody
                $ setRequestHeader "Authorization" ["Bearer " <> TE.encodeUtf8 apiKey']
                $ request'

    withResponse request $ \response -> do
      firstChunkRef <- newIORef True
      runConduit $ responseBody response .| CC.linesUnboundedAscii .| parseSSE .| CL.mapM_ (processChunk firstChunkRef)
      return ()

  case result of
    Left (e :: SomeException) -> return $ Left $ NetworkError (show e)
    Right () -> return $ Right $ LLMResponse ""

parseSSE :: Monad m => ConduitT BS.ByteString BS.ByteString m ()
parseSSE = CL.mapMaybe extractData
  where
    extractData line
      | "data: " `BS.isPrefixOf` line = Just $ BS.drop 6 line
      | otherwise = Nothing

processChunk :: IORef Bool -> BS.ByteString -> IO ()
processChunk firstChunkRef chunk
  | chunk == "[DONE]" = return ()
  | BS.null chunk = return ()
  | otherwise = case decode (LBS.fromStrict chunk) of
      Just (streamResp :: OpenAIStreamResponse) ->
        case streamChoices streamResp of
          (choice:_) -> case deltaContent (delta choice) of
            Just txt -> do
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
          [] -> return ()
      Nothing -> return ()  -- Silently ignore parse errors
