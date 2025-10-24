{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Providers.Claude (claudeProvider) where

import Control.Exception (try, SomeException)
import Control.Monad (unless, when, guard)
import Data.Foldable (forM_)
import Data.Aeson (FromJSON(..), ToJSON(..), Value, object, (.=), (.:), (.:?), withObject, eitherDecode, encode)
import Data.Bool (bool)
import Data.List (partition)
import Data.Maybe (fromMaybe)
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
import System.IO (hFlush, stdout, stderr)
import Network.HTTP.Simple
import Network.HTTP.Conduit (responseBody)
import qualified Core.Types as Types
import Core.Types (LLMRequest, LLMResponse(..), LLMError(..), LLMProvider(..))
import UI.Spinner (stopSpinner)

-- Tool types
data ClaudeTool = ClaudeTool
  { toolName :: Text
  , toolDescription :: Text
  , toolInputSchema :: Value
  } deriving (Show)

instance ToJSON ClaudeTool where
  toJSON (ClaudeTool n d s) =
    object ["name" .= n, "description" .= d, "input_schema" .= s]

data ClaudeToolUse = ClaudeToolUse
  { toolUseId :: Text
  , toolUseName :: Text
  , toolUseInput :: Value
  } deriving (Show)

instance FromJSON ClaudeToolUse where
  parseJSON = withObject "ClaudeToolUse" $ \v ->
    ClaudeToolUse <$> v .: "id" <*> v .: "name" <*> v .: "input"

-- Message types
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
  , systemPrompt :: Maybe Text
  , tools :: Maybe [ClaudeTool]
  } deriving (Show)

instance ToJSON ClaudeRequest where
  toJSON (ClaudeRequest m msgs mt s sys ts) =
    let base = ["model" .= m, "messages" .= msgs, "max_tokens" .= mt, "stream" .= s]
        withSys = maybe [] (\sysText -> ["system" .= sysText]) sys
        withTools = maybe [] (\toolList -> ["tools" .= toolList]) ts
    in object (base <> withSys <> withTools)

-- Content types for response
data ClaudeContentBlock
  = ClaudeTextBlock Text
  | ClaudeToolUseBlock ClaudeToolUse
  deriving (Show)

instance FromJSON ClaudeContentBlock where
  parseJSON = withObject "ClaudeContentBlock" $ \v -> do
    blockType <- v .: "type"
    case blockType :: Text of
      "text" -> ClaudeTextBlock <$> v .: "text"
      "tool_use" -> do
        tId <- v .: "id"
        tName <- v .: "name"
        tInput <- v .: "input"
        return $ ClaudeToolUseBlock (ClaudeToolUse tId tName tInput)
      _ -> fail $ "Unknown content block type: " ++ T.unpack blockType

newtype ClaudeResponse = ClaudeResponse
  { responseContent :: [ClaudeContentBlock]
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

-- | Create Claude provider
claudeProvider :: LLMProvider
claudeProvider = LLMProvider
  { callLLM = callClaude
  }

-- Convert MCP tools to Claude format
mcpToolsToClaude :: Types.MCPContext -> [ClaudeTool]
mcpToolsToClaude ctx =
  [ ClaudeTool name desc schema
  | (name, desc, schema) <- Types.mcpTools ctx
  ]

callClaude :: LLMRequest -> IO (Either LLMError LLMResponse)
callClaude llmReq = case Types.apiKey llmReq of
  Nothing -> return $ Left $ ConfigError "Claude API key is required"
  Just apiKey' -> do
    let model' = fromMaybe "claude-3-5-sonnet-20241022" (Types.model llmReq)
        -- Extract system messages and convert to system prompt
        (systemPrompt', userMessages) = extractSystemMessages (Types.history llmReq)
        -- Build messages from non-system history + current prompt
        historyMessages = map toClaudeMessage userMessages
        allMessages = historyMessages ++ [ClaudeMessage "user" (Types.prompt llmReq)]
        -- Convert MCP tools to Claude format
        tools' = Types.mcpContext llmReq >>= \ctx ->
          guard (not $ null $ Types.mcpTools ctx) >> Just (mcpToolsToClaude ctx)

    bool (callClaudeNonStream apiKey' model' allMessages systemPrompt' tools')
         (callClaudeStream apiKey' model' allMessages systemPrompt' tools')
         (Types.streaming llmReq)
  where
    toClaudeMessage msg = ClaudeMessage (Types.role msg) (Types.messageContent msg)

    -- Extract system messages and combine them into a single system prompt
    extractSystemMessages :: [Types.Message] -> (Maybe Text, [Types.Message])
    extractSystemMessages msgs =
      let (systemMsgs, nonSystemMsgs) = partition (\m -> Types.role m == "system") msgs
          systemPrompt' = case map Types.messageContent systemMsgs of
            [] -> Nothing
            xs -> Just $ T.intercalate "\n\n" xs
      in (systemPrompt', nonSystemMsgs)

-- | Build Claude request
buildClaudeRequest :: Text -> Text -> [ClaudeMessage] -> Maybe Text -> Maybe [ClaudeTool] -> Bool -> IO Request
buildClaudeRequest apiKey' model' messages' systemPrompt' tools' stream' = do
  let requestBody = ClaudeRequest
        { requestModel = model'
        , messages = messages'
        , maxTokens = 4096
        , requestStream = stream'
        , systemPrompt = systemPrompt'
        , tools = tools'
        }
  request' <- parseRequest "POST https://api.anthropic.com/v1/messages"
  return $ setRequestBodyJSON requestBody
         $ setRequestHeader "x-api-key" [TE.encodeUtf8 apiKey']
         $ setRequestHeader "anthropic-version" ["2023-06-01"]
         $ setRequestHeader "content-type" ["application/json"] request'

callClaudeNonStream :: Text -> Text -> [ClaudeMessage] -> Maybe Text -> Maybe [ClaudeTool] -> IO (Either LLMError LLMResponse)
callClaudeNonStream apiKey' model' messages' systemPrompt' tools' = do
  result <- try $ executeClaudeRequest apiKey' model' messages' systemPrompt' tools'
  case result of
    Left (e :: SomeException) -> return $ Left $ NetworkError $ T.pack $ show e
    Right res -> return res

executeClaudeRequest :: Text -> Text -> [ClaudeMessage] -> Maybe Text -> Maybe [ClaudeTool] -> IO (Either LLMError LLMResponse)
executeClaudeRequest apiKey' model' messages' systemPrompt' tools' = do
  request <- buildClaudeRequest apiKey' model' messages' systemPrompt' tools' False
  response <- httpLBS request
  let body = getResponseBody response
      statusCode = getResponseStatusCode response
  when (statusCode /= 200) $ printErrorResponse statusCode body
  parseClaudeResponse body statusCode

printErrorResponse :: Int -> LBS.ByteString -> IO ()
printErrorResponse statusCode body = do
  TIO.hPutStrLn stderr $ "Claude API Error (status " <> T.pack (show statusCode) <> "):"
  LBS.hPutStr stderr body
  TIO.hPutStrLn stderr ""

parseClaudeResponse :: LBS.ByteString -> Int -> IO (Either LLMError LLMResponse)
parseClaudeResponse body statusCode =
  case eitherDecode body of
    Left err -> return $ Left $ ParseError $ T.pack $ err ++ " (status: " ++ show statusCode ++ ")"
    Right claudeResp -> do
      let (textContent', toolCalls') = extractContent (responseContent claudeResp)
      return $ Right $ LLMResponse textContent' toolCalls'

extractContent :: [ClaudeContentBlock] -> (Text, Maybe [Types.ToolCall])
extractContent blocks =
  let texts = [t | ClaudeTextBlock t <- blocks]
      toolUseBlocks = [tu | ClaudeToolUseBlock tu <- blocks]
      textContent' = T.intercalate "\n" texts
      toolCalls' = if null toolUseBlocks
                   then Nothing
                   else Just $ map convertToolUse toolUseBlocks
  in (textContent', toolCalls')

convertToolUse :: ClaudeToolUse -> Types.ToolCall
convertToolUse tu = Types.ToolCall
  (toolUseId tu)
  (toolUseName tu)
  (TE.decodeUtf8 $ LBS.toStrict $ encode $ toolUseInput tu)

callClaudeStream :: Text -> Text -> [ClaudeMessage] -> Maybe Text -> Maybe [ClaudeTool] -> IO (Either LLMError LLMResponse)
callClaudeStream apiKey' model' messages' systemPrompt' tools' = do
  result <- try $ do
    request <- buildClaudeRequest apiKey' model' messages' systemPrompt' tools' True
    withResponse request $ \response -> do
      firstChunkRef <- newIORef True
      runConduit $ responseBody response
        .| CC.linesUnboundedAscii
        .| CL.mapMaybe extractData
        .| CL.mapM_ (processClaudeChunk firstChunkRef)
      return ()

  case result of
    Left (e :: SomeException) -> return $ Left $ NetworkError $ T.pack $ show e
    Right () -> return $ Right $ LLMResponse "" Nothing
  where
    extractData line
      | "data: " `BS.isPrefixOf` line = Just $ BS.drop 6 line
      | otherwise = Nothing

processClaudeChunk :: IORef Bool -> BS.ByteString -> IO ()
processClaudeChunk firstChunkRef chunk
  | BS.null chunk = return ()
  | otherwise = case eitherDecode (LBS.fromStrict chunk) of
      Right (event :: ClaudeStreamEvent) -> handleClaudeStreamEvent firstChunkRef chunk event
      Left _ -> return () -- Silently ignore parse errors

handleClaudeStreamEvent :: IORef Bool -> BS.ByteString -> ClaudeStreamEvent -> IO ()
handleClaudeStreamEvent firstChunkRef chunk event =
  case eventType event of
    "error" -> printStreamError chunk
    "content_block_delta" -> handleContentBlockDelta firstChunkRef event
    _ -> return ()

printStreamError :: BS.ByteString -> IO ()
printStreamError chunk = do
  TIO.hPutStrLn stderr "Claude API Error in stream:"
  LBS.hPutStr stderr (LBS.fromStrict chunk)
  TIO.hPutStrLn stderr ""

handleContentBlockDelta :: IORef Bool -> ClaudeStreamEvent -> IO ()
handleContentBlockDelta firstChunkRef event =
  forM_ (eventDelta event) (handleDeltaText firstChunkRef)

handleDeltaText :: IORef Bool -> ClaudeStreamDelta -> IO ()
handleDeltaText firstChunkRef delta =
  forM_ (deltaText delta) (outputDeltaText firstChunkRef)

outputDeltaText :: IORef Bool -> Text -> IO ()
outputDeltaText firstChunkRef txt =
  unless (T.null txt) $ do
    isFirst <- readIORef firstChunkRef
    when isFirst $ do
      writeIORef firstChunkRef False
      stopSpinner
    TIO.putStr txt
    hFlush stdout
