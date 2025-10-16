{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Providers.Ollama (ollamaProvider) where

import Control.Exception (try, SomeException)
import Control.Monad (unless, when)
import Data.Aeson (FromJSON(..), ToJSON(..), Value, object, (.=), (.:), (.:?), withObject, eitherDecode, encode)
import qualified Data.Maybe
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit ((.|), runConduit)
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import System.IO (hFlush, stdout)
import Network.HTTP.Simple
import Network.HTTP.Conduit (responseBody)
import qualified Core.Types as Types
import Core.Types (LLMRequest, LLMResponse(..), LLMError(..), LLMProvider(..))
import UI.Spinner (stopSpinner)

-- Tool types
data OllamaTool = OllamaTool
  { ollamaToolType :: Text
  , ollamaToolFunction :: OllamaFunction
  } deriving (Show)

instance ToJSON OllamaTool where
  toJSON (OllamaTool t f) =
    object ["type" .= t, "function" .= f]

data OllamaFunction = OllamaFunction
  { ollamaFunctionName :: Text
  , ollamaFunctionDescription :: Text
  , ollamaFunctionParameters :: Value
  } deriving (Show)

instance ToJSON OllamaFunction where
  toJSON (OllamaFunction n d p) =
    object ["name" .= n, "description" .= d, "parameters" .= p]

newtype OllamaToolCall = OllamaToolCall
  { ollamaToolCallFunction :: OllamaToolCallFunction
  } deriving (Show)

instance FromJSON OllamaToolCall where
  parseJSON = withObject "OllamaToolCall" $ \v ->
    OllamaToolCall <$> v .: "function"

instance ToJSON OllamaToolCall where
  toJSON (OllamaToolCall f) =
    object ["function" .= f]

data OllamaToolCallFunction = OllamaToolCallFunction
  { tcfName :: Text
  , tcfArguments :: Value
  } deriving (Show)

instance FromJSON OllamaToolCallFunction where
  parseJSON = withObject "OllamaToolCallFunction" $ \v ->
    OllamaToolCallFunction <$> v .: "name" <*> v .: "arguments"

instance ToJSON OllamaToolCallFunction where
  toJSON (OllamaToolCallFunction n a) =
    object ["name" .= n, "arguments" .= a]

-- Message types
data OllamaChatMessage = OllamaChatMessage
  { msgRole :: Text
  , msgContent :: Text
  , msgToolCalls :: Maybe [OllamaToolCall]
  } deriving (Show)

instance ToJSON OllamaChatMessage where
  toJSON (OllamaChatMessage r c Nothing) =
    object ["role" .= r, "content" .= c]
  toJSON (OllamaChatMessage r c (Just tcs)) =
    object ["role" .= r, "content" .= c, "tool_calls" .= tcs]

-- Chat request (for tool support)
data OllamaChatRequest = OllamaChatRequest
  { chatModel :: Text
  , chatMessages :: [OllamaChatMessage]
  , chatStream :: Bool
  , chatTools :: Maybe [OllamaTool]
  } deriving (Show)

instance ToJSON OllamaChatRequest where
  toJSON (OllamaChatRequest m msgs s Nothing) =
    object ["model" .= m, "messages" .= msgs, "stream" .= s]
  toJSON (OllamaChatRequest m msgs s (Just ts)) =
    object ["model" .= m, "messages" .= msgs, "stream" .= s, "tools" .= ts]

-- Generate request (for non-tool support)
data OllamaGenerateRequest = OllamaGenerateRequest
  { requestModel :: Text
  , ollamaPrompt :: Text
  , requestStream :: Bool
  } deriving (Show)

instance ToJSON OllamaGenerateRequest where
  toJSON (OllamaGenerateRequest m p s) =
    object ["model" .= m, "prompt" .= p, "stream" .= s]

-- Response types
data OllamaChatResponseMessage = OllamaChatResponseMessage
  { respMsgContent :: Text
  , respMsgThinking :: Maybe Text
  , respMsgToolCalls :: Maybe [OllamaToolCall]
  } deriving (Show)

instance FromJSON OllamaChatResponseMessage where
  parseJSON = withObject "OllamaChatResponseMessage" $ \v ->
    OllamaChatResponseMessage
      <$> v .: "content"
      <*> v .:? "thinking"
      <*> v .:? "tool_calls"

newtype OllamaChatResponse = OllamaChatResponse
  { chatRespMessage :: OllamaChatResponseMessage
  } deriving (Show)

instance FromJSON OllamaChatResponse where
  parseJSON = withObject "OllamaChatResponse" $ \v ->
    OllamaChatResponse <$> v .: "message"

newtype OllamaGenerateResponse = OllamaGenerateResponse
  { ollamaResponse :: Text
  } deriving (Show)

instance FromJSON OllamaGenerateResponse where
  parseJSON = withObject "OllamaGenerateResponse" $ \v ->
    OllamaGenerateResponse <$> v .: "response"

-- | Create Ollama provider
ollamaProvider :: LLMProvider
ollamaProvider = LLMProvider
  { callLLM = callOllama
  }

-- Convert MCP tools to Ollama format
mcpToolsToOllama :: Types.MCPContext -> [OllamaTool]
mcpToolsToOllama ctx =
  [ OllamaTool "function" (OllamaFunction name desc schema)
  | (name, desc, schema) <- Types.mcpTools ctx
  ]

callOllama :: LLMRequest -> IO (Either LLMError LLMResponse)
callOllama llmReq = do
  let model' = Data.Maybe.fromMaybe "llama3.2" (Types.model llmReq)
      baseUrl' = Data.Maybe.fromMaybe "localhost:11434" (Types.baseUrl llmReq)
      -- Convert MCP tools to Ollama format
      tools' = case Types.mcpContext llmReq of
        Nothing -> Nothing
        Just ctx -> if null (Types.mcpTools ctx) then Nothing else Just $ mcpToolsToOllama ctx

  -- Use chat endpoint if tools are present, otherwise use generate
  case tools' of
    Just ts -> callOllamaChat baseUrl' model' llmReq ts
    Nothing -> callOllamaGenerate baseUrl' model' llmReq

-- Call Ollama chat endpoint (with tool support)
callOllamaChat :: Text -> Text -> LLMRequest -> [OllamaTool] -> IO (Either LLMError LLMResponse)
callOllamaChat baseUrl' model' llmReq tools' = do
  let historyMessages = map (\msg -> OllamaChatMessage (Types.role msg) (Types.messageContent msg) Nothing) (Types.history llmReq)
      allMessages = historyMessages ++ [OllamaChatMessage "user" (Types.prompt llmReq) Nothing]

  if Types.streaming llmReq
    then callOllamaChatStream baseUrl' model' allMessages tools'
    else callOllamaChatNonStream baseUrl' model' allMessages tools'

callOllamaChatNonStream :: Text -> Text -> [OllamaChatMessage] -> [OllamaTool] -> IO (Either LLMError LLMResponse)
callOllamaChatNonStream baseUrl' model' messages' tools' = do
  result <- try $ do
    let requestBody = OllamaChatRequest
          { chatModel = model'
          , chatMessages = messages'
          , chatStream = False
          , chatTools = Just tools'
          }
    request' <- parseRequest $ "POST http://" <> T.unpack baseUrl' <> "/api/chat"
    let request = setRequestBodyJSON requestBody request'
    response <- httpLBS request
    let body = getResponseBody response
    case eitherDecode body of
      Left err -> return $ Left $ ParseError err
      Right chatResp -> do
        let msg = chatRespMessage chatResp
            content' = respMsgContent msg
            toolCalls' = case respMsgToolCalls msg of
              Nothing -> Nothing
              Just tcs -> Just $ zipWith convertToolCall [1..] tcs
        return $ Right $ LLMResponse content' toolCalls'

  case result of
    Left (e :: SomeException) -> return $ Left $ NetworkError (show e)
    Right res -> return res
  where
    convertToolCall :: Int -> OllamaToolCall -> Types.ToolCall
    convertToolCall idx tc = Types.ToolCall
      (T.pack $ "call_" <> show idx)
      (tcfName $ ollamaToolCallFunction tc)
      (TE.decodeUtf8 $ LBS.toStrict $ encode $ tcfArguments $ ollamaToolCallFunction tc)

callOllamaChatStream :: Text -> Text -> [OllamaChatMessage] -> [OllamaTool] -> IO (Either LLMError LLMResponse)
callOllamaChatStream baseUrl' model' messages' tools' = do
  result <- try $ do
    let requestBody = OllamaChatRequest
          { chatModel = model'
          , chatMessages = messages'
          , chatStream = True
          , chatTools = Just tools'
          }
    request' <- parseRequest $ "POST http://" <> T.unpack baseUrl' <> "/api/chat"
    let request = setRequestBodyJSON requestBody request'
    withResponse request $ \resp -> do
      firstChunkRef <- newIORef True
      toolCallsRef <- newIORef Nothing
      runConduit $ responseBody resp
        .| CC.linesUnboundedAscii
        .| CL.mapM_ (processChatChunkWithTools firstChunkRef toolCallsRef)
      -- Read collected tool calls
      readIORef toolCallsRef

  case result of
    Left (e :: SomeException) -> return $ Left $ NetworkError (show e)
    Right toolCalls' -> return $ Right $ LLMResponse "" toolCalls'

processChatChunkWithTools :: IORef Bool -> IORef (Maybe [Types.ToolCall]) -> BS.ByteString -> IO ()
processChatChunkWithTools firstChunkRef toolCallsRef chunk
  | BS.null chunk = return ()
  | otherwise = case eitherDecode (LBS.fromStrict chunk) of
      Right (chatResp :: OllamaChatResponse) -> do
        let msg = chatRespMessage chatResp
            txt = respMsgContent msg
        -- Output text content
        unless (T.null txt) $ do
          isFirst <- readIORef firstChunkRef
          when isFirst $ do
            writeIORef firstChunkRef False
            stopSpinner
          TIO.putStr txt
          hFlush stdout
        -- Collect tool calls
        case respMsgToolCalls msg of
          Just tcs -> do
            let convertedCalls = zipWith convertToolCall [1..] tcs
            writeIORef toolCallsRef (Just convertedCalls)
          Nothing -> return ()
      Left _ -> return ()
  where
    convertToolCall :: Int -> OllamaToolCall -> Types.ToolCall
    convertToolCall idx tc = Types.ToolCall
      (T.pack $ "call_" <> show idx)
      (tcfName $ ollamaToolCallFunction tc)
      (TE.decodeUtf8 $ LBS.toStrict $ encode $ tcfArguments $ ollamaToolCallFunction tc)

-- Call Ollama generate endpoint (without tool support)
callOllamaGenerate :: Text -> Text -> LLMRequest -> IO (Either LLMError LLMResponse)
callOllamaGenerate baseUrl' model' llmReq = do
  let historyText = T.intercalate "\n" $ map (\msg ->
        Types.role msg <> ": " <> Types.messageContent msg) (Types.history llmReq)
      fullPrompt = if T.null historyText
                   then Types.prompt llmReq
                   else historyText <> "\nuser: " <> Types.prompt llmReq

  if Types.streaming llmReq
    then callOllamaGenerateStream baseUrl' model' fullPrompt
    else callOllamaGenerateNonStream baseUrl' model' fullPrompt

callOllamaGenerateNonStream :: Text -> Text -> Text -> IO (Either LLMError LLMResponse)
callOllamaGenerateNonStream baseUrl' model' prompt' = do
  result <- try $ do
    let requestBody = OllamaGenerateRequest
          { requestModel = model'
          , ollamaPrompt = prompt'
          , requestStream = False
          }
    request' <- parseRequest $ "POST http://" <> T.unpack baseUrl' <> "/api/generate"
    let request = setRequestBodyJSON requestBody request'
    response <- httpLBS request
    let body = getResponseBody response
    case eitherDecode body of
      Left err -> return $ Left $ ParseError err
      Right genResp -> return $ Right $ LLMResponse (ollamaResponse genResp) Nothing

  case result of
    Left (e :: SomeException) -> return $ Left $ NetworkError (show e)
    Right res -> return res

callOllamaGenerateStream :: Text -> Text -> Text -> IO (Either LLMError LLMResponse)
callOllamaGenerateStream baseUrl' model' prompt' = do
  result <- try $ do
    let requestBody = OllamaGenerateRequest
          { requestModel = model'
          , ollamaPrompt = prompt'
          , requestStream = True
          }
    request' <- parseRequest $ "POST http://" <> T.unpack baseUrl' <> "/api/generate"
    let request = setRequestBodyJSON requestBody request'
    withResponse request $ \resp -> do
      firstChunkRef <- newIORef True
      runConduit $ responseBody resp
        .| CC.linesUnboundedAscii
        .| CL.mapM_ (processGenerateChunk firstChunkRef)
      return ()

  case result of
    Left (e :: SomeException) -> return $ Left $ NetworkError (show e)
    Right () -> return $ Right $ LLMResponse "" Nothing

processGenerateChunk :: IORef Bool -> BS.ByteString -> IO ()
processGenerateChunk firstChunkRef chunk
  | BS.null chunk = return ()
  | otherwise = case eitherDecode (LBS.fromStrict chunk) of
      Right (genResp :: OllamaGenerateResponse) -> do
        let txt = ollamaResponse genResp
        unless (T.null txt) $ do
          isFirst <- readIORef firstChunkRef
          when isFirst $ do
            writeIORef firstChunkRef False
            stopSpinner
        TIO.putStr txt
        hFlush stdout
      Left _ -> return ()
