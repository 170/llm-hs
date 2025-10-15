{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLM.Claude (callClaude) where

import Control.Exception (try, SomeException)
import Control.Monad (when)
import Data.IORef (newIORef, readIORef, writeIORef, IORef)
import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?), withObject, decode, eitherDecode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit ((.|), runConduit, ConduitT)
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import Network.HTTP.Simple
import Network.HTTP.Conduit (responseBody, Response)
import Network.HTTP.Types.Status (statusCode)
import qualified LLM.Types as Types
import LLM.Types (LLMRequest, LLMResponse(..), LLMError(..))
import LLM.Spinner (stopSpinner)

data ClaudeMessage = ClaudeMessage
  { role :: Text
  , messageContent :: Text
  } deriving (Show)

instance ToJSON ClaudeMessage where
  toJSON (ClaudeMessage r c) = object ["role" .= r, "content" .= c]

data ClaudeRequest = ClaudeRequest
  { requestModel :: Text
  , messages :: [ClaudeMessage]
  , maxTokens :: Int
  , requestStream :: Bool
  } deriving (Show)

instance ToJSON ClaudeRequest where
  toJSON (ClaudeRequest m msgs mt s) =
    object ["model" .= m, "messages" .= msgs, "max_tokens" .= mt, "stream" .= s]

data ClaudeContent = ClaudeContent
  { contentText :: Text
  } deriving (Show)

instance FromJSON ClaudeContent where
  parseJSON = withObject "ClaudeContent" $ \v ->
    ClaudeContent <$> v .: "text"

data ClaudeResponse = ClaudeResponse
  { responseContent :: [ClaudeContent]
  } deriving (Show)

instance FromJSON ClaudeResponse where
  parseJSON = withObject "ClaudeResponse" $ \v ->
    ClaudeResponse <$> v .: "content"

-- Streaming types
data ClaudeStreamDelta = ClaudeStreamDelta
  { deltaType :: Text
  , deltaText :: Maybe Text
  } deriving (Show)

instance FromJSON ClaudeStreamDelta where
  parseJSON = withObject "ClaudeStreamDelta" $ \v ->
    ClaudeStreamDelta <$> v .: "type" <*> v .:? "text"

data ClaudeStreamEvent = ClaudeStreamEvent
  { eventType :: Text
  , eventDelta :: Maybe ClaudeStreamDelta
  } deriving (Show)

instance FromJSON ClaudeStreamEvent where
  parseJSON = withObject "ClaudeStreamEvent" $ \v ->
    ClaudeStreamEvent <$> v .: "type" <*> v .:? "delta"

-- Error response types
data ClaudeErrorDetail = ClaudeErrorDetail
  { errorType :: Text
  , errorMessage :: Text
  } deriving (Show)

instance FromJSON ClaudeErrorDetail where
  parseJSON = withObject "ClaudeErrorDetail" $ \v ->
    ClaudeErrorDetail <$> v .: "type" <*> v .: "message"

data ClaudeErrorResponse = ClaudeErrorResponse
  { errorDetail :: ClaudeErrorDetail
  } deriving (Show)

instance FromJSON ClaudeErrorResponse where
  parseJSON = withObject "ClaudeErrorResponse" $ \v ->
    ClaudeErrorResponse <$> v .: "error"

callClaude :: LLMRequest -> IO (Either LLMError LLMResponse)
callClaude llmReq = do
  let apiKey' = case Types.apiKey llmReq of
        Nothing -> error "Claude API key is required"
        Just k -> k
      model' = case Types.model llmReq of
        Nothing -> "claude-3-5-sonnet-20241022"
        Just m -> m
      -- Build messages from history + current prompt
      historyMessages = map (\msg -> ClaudeMessage (Types.role msg) (Types.messageContent msg)) (Types.history llmReq)
      allMessages = historyMessages ++ [ClaudeMessage "user" (Types.prompt llmReq)]

  if Types.streaming llmReq
    then callClaudeStream apiKey' model' allMessages
    else callClaudeNonStream apiKey' model' allMessages

callClaudeNonStream :: Text -> Text -> [ClaudeMessage] -> IO (Either LLMError LLMResponse)
callClaudeNonStream apiKey' model' messages' = do
  result <- try $ do
    let requestBody = ClaudeRequest
          { requestModel = model'
          , messages = messages'
          , maxTokens = 4096
          , requestStream = False
          }

    request' <- parseRequest "POST https://api.anthropic.com/v1/messages"
    let request = setRequestBodyJSON requestBody
                $ setRequestHeader "x-api-key" [TE.encodeUtf8 apiKey']
                $ setRequestHeader "anthropic-version" ["2023-06-01"]
                $ setRequestHeader "content-type" ["application/json"]
                $ request'

    response <- httpLBS request
    let status = statusCode $ getResponseStatus response
        body = getResponseBody response

    if status >= 400
      then case eitherDecode body :: Either String ClaudeErrorResponse of
        Right errResp -> return $ Left $ APIError $ T.unpack (errorMessage (errorDetail errResp))
        Left _ -> return $ Left $ APIError $ "HTTP " ++ show status ++ ": " ++ show body
      else case eitherDecode body :: Either String ClaudeResponse of
        Right claudeResp ->
          case responseContent claudeResp of
            [] -> return $ Left $ APIError "No response from Claude"
            (c:_) -> return $ Right $ LLMResponse (contentText c)
        Left err -> return $ Left $ APIError $ "Parse error: " ++ err

  case result of
    Left (e :: SomeException) -> return $ Left $ NetworkError (show e)
    Right res -> return res

callClaudeStream :: Text -> Text -> [ClaudeMessage] -> IO (Either LLMError LLMResponse)
callClaudeStream apiKey' model' messages' = do
  result <- try $ do
    let requestBody = ClaudeRequest
          { requestModel = model'
          , messages = messages'
          , maxTokens = 4096
          , requestStream = True
          }

    request' <- parseRequest "POST https://api.anthropic.com/v1/messages"
    let request = setRequestBodyJSON requestBody
                $ setRequestHeader "x-api-key" [TE.encodeUtf8 apiKey']
                $ setRequestHeader "anthropic-version" ["2023-06-01"]
                $ setRequestHeader "content-type" ["application/json"]
                $ request'

    withResponse request $ \response -> do
      firstChunkRef <- newIORef True
      runConduit $ responseBody response .| CC.linesUnboundedAscii .| parseClaudeSSE .| CL.mapM_ (processClaudeChunk firstChunkRef)
      return ()

  case result of
    Left (e :: SomeException) -> return $ Left $ NetworkError (show e)
    Right () -> return $ Right $ LLMResponse ""

parseClaudeSSE :: Monad m => ConduitT BS.ByteString BS.ByteString m ()
parseClaudeSSE = CL.mapMaybe extractData
  where
    extractData line
      | "data: " `BS.isPrefixOf` line = Just $ BS.drop 6 line
      | otherwise = Nothing

processClaudeChunk :: IORef Bool -> BS.ByteString -> IO ()
processClaudeChunk firstChunkRef chunk
  | BS.null chunk = return ()
  | otherwise = case decode (LBS.fromStrict chunk) of
      Just (event :: ClaudeStreamEvent) ->
        case eventType event of
          "content_block_delta" ->
            case eventDelta event of
              Just delta ->
                case deltaText delta of
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
              Nothing -> return ()
          _ -> return ()
      Nothing -> return ()
