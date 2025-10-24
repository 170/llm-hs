{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Providers.OpenAI (openAIProvider) where

import Control.Exception (try, SomeException)
import Control.Monad (unless, when, guard)
import Data.Foldable (forM_)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Aeson (FromJSON(..), ToJSON(..), Value, object, (.=), (.:), (.:?), withObject, eitherDecode)
import Control.Applicative ((<|>))
import Data.Bool (bool)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit ((.|), runConduit)
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import Network.HTTP.Simple
import Network.HTTP.Conduit (responseBody)
import qualified Core.Types as Types
import Core.Types (LLMRequest, LLMResponse(..), LLMError(..), LLMProvider(..))
import UI.Spinner (stopSpinner)

data OpenAIToolCall = OpenAIToolCall
  { toolCallIndex :: Maybe Int
  , toolCallId :: Text
  , toolCallType :: Text
  , toolCallFunction :: OpenAIToolCallFunction
  } deriving (Show)

data OpenAIToolCallFunction = OpenAIToolCallFunction
  { toolCallFunctionName :: Text
  , toolCallFunctionArguments :: Text
  } deriving (Show)

instance ToJSON OpenAIToolCall where
  toJSON (OpenAIToolCall _ tid ttype func) =
    object ["id" .= tid, "type" .= ttype, "function" .= func]

instance FromJSON OpenAIToolCall where
  parseJSON = withObject "OpenAIToolCall" $ \v ->
    OpenAIToolCall
      <$> v .:? "index"
      <*> (v .: "id" <|> pure "")
      <*> (v .: "type" <|> pure "function")
      <*> v .: "function"

instance ToJSON OpenAIToolCallFunction where
  toJSON (OpenAIToolCallFunction name args) =
    object ["name" .= name, "arguments" .= args]

instance FromJSON OpenAIToolCallFunction where
  parseJSON = withObject "OpenAIToolCallFunction" $ \v ->
    OpenAIToolCallFunction
      <$> (v .: "name" <|> pure "")
      <*> (v .: "arguments" <|> pure "")

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

newtype OpenAIChoice = OpenAIChoice
  { message :: OpenAIMessage
  } deriving (Show)

instance FromJSON OpenAIChoice where
  parseJSON = withObject "OpenAIChoice" $ \v ->
    OpenAIChoice <$> v .: "message"

newtype OpenAIResponse = OpenAIResponse
  { choices :: [OpenAIChoice]
  } deriving (Show)

instance FromJSON OpenAIResponse where
  parseJSON = withObject "OpenAIResponse" $ \v ->
    OpenAIResponse <$> v .: "choices"

-- Streaming types
data OpenAIDelta = OpenAIDelta
  { deltaContent :: Maybe Text
  , deltaToolCalls :: Maybe [OpenAIToolCall]
  } deriving (Show)

instance FromJSON OpenAIDelta where
  parseJSON = withObject "OpenAIDelta" $ \v ->
    OpenAIDelta
      <$> v .:? "content"
      <*> v .:? "tool_calls"

newtype OpenAIStreamChoice = OpenAIStreamChoice
  { delta :: OpenAIDelta
  } deriving (Show)

instance FromJSON OpenAIStreamChoice where
  parseJSON = withObject "OpenAIStreamChoice" $ \v ->
    OpenAIStreamChoice <$> v .: "delta"

newtype OpenAIStreamResponse = OpenAIStreamResponse
  { streamChoices :: [OpenAIStreamChoice]
  } deriving (Show)

instance FromJSON OpenAIStreamResponse where
  parseJSON = withObject "OpenAIStreamResponse" $ \v ->
    OpenAIStreamResponse <$> v .: "choices"

-- Accumulator for streaming tool calls
data ToolCallAccumulator = ToolCallAccumulator
  { accId :: Text
  , accName :: Text
  , accArguments :: Text
  } deriving (Show)

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
callOpenAI llmReq = case Types.apiKey llmReq of
  Nothing -> return $ Left $ ConfigError "OpenAI API key is required"
  Just apiKey' -> do
    let model' = fromMaybe "gpt-4o-mini" (Types.model llmReq)
        -- Build messages from history + current prompt
        historyMessages = map toOpenAIMessage (Types.history llmReq)
        allMessages = historyMessages ++ [OpenAIMessage "user" (Just $ Types.prompt llmReq) Nothing]
        -- Convert MCP tools to OpenAI format
        tools' = Types.mcpContext llmReq >>= \ctx ->
          guard (not $ null $ Types.mcpTools ctx) >> Just (mcpToolsToOpenAI ctx)

    bool (callOpenAINonStream apiKey' model' allMessages tools')
         (callOpenAIStream apiKey' model' allMessages tools')
         (Types.streaming llmReq)
  where
    toOpenAIMessage msg = OpenAIMessage (Types.role msg) (Just $ Types.messageContent msg) Nothing

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
         $ setRequestHeader "Authorization" ["Bearer " <> TE.encodeUtf8 apiKey'] request'

callOpenAINonStream :: Text -> Text -> [OpenAIMessage] -> Maybe [OpenAITool] -> IO (Either LLMError LLMResponse)
callOpenAINonStream apiKey' model' messages' tools' = do
  result <- try $ do
    request <- buildOpenAIRequest apiKey' model' messages' tools' False
    response <- httpLBS request
    let body = getResponseBody response
    case eitherDecode body of
      Left err -> return $ Left $ ParseError $ T.pack err
      Right openAIResp ->
        case choices openAIResp of
          [] -> return $ Left $ APIError "No response from OpenAI"
          (choice:_) ->
            let msg = message choice
                content' = fromMaybe "" (messageContent msg)
                toolCalls' = (fmap . fmap) convertToolCall (Providers.OpenAI.toolCalls msg)
            in return $ Right $ LLMResponse content' toolCalls'

  case result of
    Left (e :: SomeException) -> return $ Left $ NetworkError $ T.pack $ show e
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
      toolCallsAccRef <- newIORef (Map.empty :: Map Int ToolCallAccumulator)
      runConduit $ responseBody response
        .| CC.linesUnboundedAscii
        .| CL.mapMaybe extractData
        .| CL.mapM_ (processChunk firstChunkRef toolCallsAccRef)
      accMap <- readIORef toolCallsAccRef
      return $ convertAccumulatorMapToToolCalls accMap

  case result of
    Left (e :: SomeException) -> return $ Left $ NetworkError $ T.pack $ show e
    Right toolCalls' -> return $ Right $ LLMResponse "" toolCalls'
  where
    extractData line
      | "data: " `BS.isPrefixOf` line = Just $ BS.drop 6 line
      | otherwise = Nothing

convertAccumulatorMapToToolCalls :: Map Int ToolCallAccumulator -> Maybe [Types.ToolCall]
convertAccumulatorMapToToolCalls accMap
  | Map.null accMap = Nothing
  | otherwise = Just $ map convertAcc $ Map.elems accMap
  where
    convertAcc acc = Types.ToolCall (accId acc) (accName acc) (accArguments acc)

processChunk :: IORef Bool -> IORef (Map Int ToolCallAccumulator) -> BS.ByteString -> IO ()
processChunk firstChunkRef toolCallsAccRef chunk
  | chunk == "[DONE]" = return ()
  | BS.null chunk = return ()
  | otherwise = case eitherDecode (LBS.fromStrict chunk) of
      Right (streamResp :: OpenAIStreamResponse) -> handleStreamResponse firstChunkRef toolCallsAccRef streamResp
      Left _ -> return () -- Silently ignore parse errors

handleStreamResponse :: IORef Bool -> IORef (Map Int ToolCallAccumulator) -> OpenAIStreamResponse -> IO ()
handleStreamResponse firstChunkRef toolCallsAccRef streamResp =
  case streamChoices streamResp of
    (choice:_) -> handleStreamChoice firstChunkRef toolCallsAccRef choice
    [] -> return ()

handleStreamChoice :: IORef Bool -> IORef (Map Int ToolCallAccumulator) -> OpenAIStreamChoice -> IO ()
handleStreamChoice firstChunkRef toolCallsAccRef choice = do
  forM_ (deltaContent (delta choice)) (outputStreamText firstChunkRef)
  -- Accumulate tool calls if present
  forM_ (deltaToolCalls (delta choice)) $ \tcs -> do
    mapM_ (accumulateToolCall toolCallsAccRef) tcs

accumulateToolCall :: IORef (Map Int ToolCallAccumulator) -> OpenAIToolCall -> IO ()
accumulateToolCall accRef tc = do
  let index = fromMaybe 0 (toolCallIndex tc)
  modifyIORef' accRef $ \accMap ->
    let currentAcc = Map.findWithDefault (ToolCallAccumulator "" "" "") index accMap
        newId = if T.null (toolCallId tc) then accId currentAcc else toolCallId tc
        func = toolCallFunction tc
        newName = if T.null (toolCallFunctionName func) then accName currentAcc else toolCallFunctionName func
        newArgs = accArguments currentAcc <> toolCallFunctionArguments func
    in Map.insert index (ToolCallAccumulator newId newName newArgs) accMap

outputStreamText :: IORef Bool -> Text -> IO ()
outputStreamText firstChunkRef txt =
  unless (T.null txt) $ do
    isFirst <- readIORef firstChunkRef
    when isFirst $ do
      writeIORef firstChunkRef False
      stopSpinner
    TIO.putStr txt
    hFlush stdout
