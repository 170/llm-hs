{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLM.OpenAI (openAIProvider) where

import Control.Exception (try, SomeException)
import Control.Monad (when)
import Data.Aeson (FromJSON(..), ToJSON(..), Value, object, (.=), (.:), (.:?), withObject, eitherDecode)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit ((.|), runConduit)
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import Network.HTTP.Simple
import Network.HTTP.Conduit (responseBody)
import qualified LLM.Types as Types
import LLM.Types (LLMRequest, LLMResponse(..), LLMError(..), LLMProvider(..))
import LLM.Spinner (stopSpinner)

data OpenAIToolCall = OpenAIToolCall
  { toolCallId :: Text
  , toolCallType :: Text
  , toolCallFunction :: OpenAIToolCallFunction
  } deriving (Show)

data OpenAIToolCallFunction = OpenAIToolCallFunction
  { toolCallFunctionName :: Text
  , toolCallFunctionArguments :: Text
  } deriving (Show)

instance ToJSON OpenAIToolCall where
  toJSON (OpenAIToolCall tid ttype func) =
    object ["id" .= tid, "type" .= ttype, "function" .= func]

instance FromJSON OpenAIToolCall where
  parseJSON = withObject "OpenAIToolCall" $ \v ->
    OpenAIToolCall
      <$> v .: "id"
      <*> v .: "type"
      <*> v .: "function"

instance ToJSON OpenAIToolCallFunction where
  toJSON (OpenAIToolCallFunction name args) =
    object ["name" .= name, "arguments" .= args]

instance FromJSON OpenAIToolCallFunction where
  parseJSON = withObject "OpenAIToolCallFunction" $ \v ->
    OpenAIToolCallFunction
      <$> v .: "name"
      <*> v .: "arguments"

data OpenAIMessage = OpenAIMessage
  { role :: Text
  , messageContent :: Maybe Text
  , toolCalls :: Maybe [OpenAIToolCall]
  } deriving (Show)

instance ToJSON OpenAIMessage where
  toJSON (OpenAIMessage r (Just c) Nothing) = object ["role" .= r, "content" .= c]
  toJSON (OpenAIMessage r Nothing (Just tc)) = object ["role" .= r, "tool_calls" .= tc]
  toJSON (OpenAIMessage r (Just c) (Just tc)) = object ["role" .= r, "content" .= c, "tool_calls" .= tc]
  toJSON (OpenAIMessage r Nothing Nothing) = object ["role" .= r, "content" .= ("" :: Text)]

instance FromJSON OpenAIMessage where
  parseJSON = withObject "OpenAIMessage" $ \v ->
    OpenAIMessage
      <$> v .: "role"
      <*> v .:? "content"
      <*> v .:? "tool_calls"

data OpenAITool = OpenAITool
  { toolType :: Text
  , function :: OpenAIFunction
  } deriving (Show)

data OpenAIFunction = OpenAIFunction
  { functionName :: Text
  , functionDescription :: Text
  , parameters :: Value
  } deriving (Show)

instance ToJSON OpenAITool where
  toJSON (OpenAITool t f) =
    object ["type" .= t, "function" .= f]

instance ToJSON OpenAIFunction where
  toJSON (OpenAIFunction n d p) =
    object ["name" .= n, "description" .= d, "parameters" .= p]

data OpenAIRequest = OpenAIRequest
  { requestModel :: Text
  , messages :: [OpenAIMessage]
  , requestStream :: Bool
  , tools :: Maybe [OpenAITool]
  } deriving (Show)

instance ToJSON OpenAIRequest where
  toJSON (OpenAIRequest m msgs s Nothing) =
    object ["model" .= m, "messages" .= msgs, "stream" .= s]
  toJSON (OpenAIRequest m msgs s (Just ts)) =
    object ["model" .= m, "messages" .= msgs, "stream" .= s, "tools" .= ts]

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

-- Convert MCP tools to OpenAI format
mcpToolsToOpenAI :: Types.MCPContext -> [OpenAITool]
mcpToolsToOpenAI ctx =
  [ OpenAITool "function" (OpenAIFunction name desc schema)
  | (name, desc, schema) <- Types.mcpTools ctx
  ]

-- | Create OpenAI provider
openAIProvider :: LLMProvider
openAIProvider = LLMProvider
  { callLLM = callOpenAI
  }

callOpenAI :: LLMRequest -> IO (Either LLMError LLMResponse)
callOpenAI llmReq = do
  let apiKey' = case Types.apiKey llmReq of
        Nothing -> error "OpenAI API key is required"
        Just k -> k
      model' = case Types.model llmReq of
        Nothing -> "gpt-4o-mini"
        Just m -> m
      -- Build messages from history + current prompt
      historyMessages = map (\msg -> OpenAIMessage (Types.role msg) (Just $ Types.messageContent msg) Nothing) (Types.history llmReq)
      allMessages = historyMessages ++ [OpenAIMessage "user" (Just $ Types.prompt llmReq) Nothing]
      -- Convert MCP tools to OpenAI format
      tools' = case Types.mcpContext llmReq of
        Nothing -> Nothing
        Just ctx -> if null (Types.mcpTools ctx) then Nothing else Just $ mcpToolsToOpenAI ctx

  if Types.streaming llmReq
    then callOpenAIStream apiKey' model' allMessages tools'
    else callOpenAINonStream apiKey' model' allMessages tools'

-- | Build OpenAI request
buildOpenAIRequest :: Text -> Text -> [OpenAIMessage] -> Maybe [OpenAITool] -> Bool -> IO Request
buildOpenAIRequest apiKey' model' messages' tools' stream' = do
  let requestBody = OpenAIRequest
        { requestModel = model'
        , messages = messages'
        , requestStream = stream'
        , tools = tools'
        }
  request' <- parseRequest "POST https://api.openai.com/v1/chat/completions"
  return $ setRequestBodyJSON requestBody
         $ setRequestHeader "Authorization" ["Bearer " <> TE.encodeUtf8 apiKey']
         $ request'

callOpenAINonStream :: Text -> Text -> [OpenAIMessage] -> Maybe [OpenAITool] -> IO (Either LLMError LLMResponse)
callOpenAINonStream apiKey' model' messages' tools' = do
  result <- try $ do
    request <- buildOpenAIRequest apiKey' model' messages' tools' False
    response <- httpLBS request
    let body = getResponseBody response
    case eitherDecode body of
      Left err -> return $ Left $ ParseError err
      Right openAIResp ->
        case choices openAIResp of
          [] -> return $ Left $ APIError "No response from OpenAI"
          (choice:_) ->
            let msg = message choice
                content' = case messageContent msg of
                  Just c -> c
                  Nothing -> ""
                toolCalls' = case LLM.OpenAI.toolCalls msg of
                  Just calls -> Just $ map convertToolCall calls
                  Nothing -> Nothing
            in return $ Right $ LLMResponse content' toolCalls'

  case result of
    Left (e :: SomeException) -> return $ Left $ NetworkError (show e)
    Right res -> return res
  where
    convertToolCall tc = Types.ToolCall
      (toolCallId tc)
      (toolCallFunctionName $ toolCallFunction tc)
      (toolCallFunctionArguments $ toolCallFunction tc)

callOpenAIStream :: Text -> Text -> [OpenAIMessage] -> Maybe [OpenAITool] -> IO (Either LLMError LLMResponse)
callOpenAIStream apiKey' model' messages' tools' = do
  result <- try $ do
    request <- buildOpenAIRequest apiKey' model' messages' tools' True
    withResponse request $ \response -> do
      firstChunkRef <- newIORef True
      runConduit $ responseBody response
        .| CC.linesUnboundedAscii
        .| CL.mapMaybe extractData
        .| CL.mapM_ (processChunk firstChunkRef)
      return ()

  case result of
    Left (e :: SomeException) -> return $ Left $ NetworkError (show e)
    Right () -> return $ Right $ LLMResponse "" Nothing
  where
    extractData line
      | "data: " `BS.isPrefixOf` line = Just $ BS.drop 6 line
      | otherwise = Nothing

processChunk :: IORef Bool -> BS.ByteString -> IO ()
processChunk firstChunkRef chunk
  | chunk == "[DONE]" = return ()
  | BS.null chunk = return ()
  | otherwise = case eitherDecode (LBS.fromStrict chunk) of
      Right (streamResp :: OpenAIStreamResponse) ->
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
      Left _ -> return ()  -- Silently ignore parse errors
