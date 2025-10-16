{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLM.Gemini (geminiProvider) where

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

-- Tool types
data GeminiTool = GeminiTool
  { toolFunctionDeclarations :: [GeminiFunctionDeclaration]
  } deriving (Show)

instance ToJSON GeminiTool where
  toJSON (GeminiTool decls) =
    object ["function_declarations" .= decls]

data GeminiFunctionDeclaration = GeminiFunctionDeclaration
  { functionName :: Text
  , functionDescription :: Text
  , functionParameters :: Value
  } deriving (Show)

instance ToJSON GeminiFunctionDeclaration where
  toJSON (GeminiFunctionDeclaration n d p) =
    object ["name" .= n, "description" .= d, "parameters" .= p]

data GeminiFunctionCall = GeminiFunctionCall
  { fcName :: Text
  , fcArgs :: Value
  } deriving (Show)

instance ToJSON GeminiFunctionCall where
  toJSON (GeminiFunctionCall n a) =
    object ["name" .= n, "args" .= a]

instance FromJSON GeminiFunctionCall where
  parseJSON = withObject "GeminiFunctionCall" $ \v ->
    GeminiFunctionCall <$> v .: "name" <*> v .: "args"

-- Content types
data GeminiPart = GeminiPart
  { partText :: Maybe Text
  , partFunctionCall :: Maybe GeminiFunctionCall
  } deriving (Show)

instance ToJSON GeminiPart where
  toJSON (GeminiPart (Just t) Nothing) = object ["text" .= t]
  toJSON (GeminiPart Nothing (Just fc)) = object ["functionCall" .= fc]
  toJSON (GeminiPart (Just t) (Just fc)) = object ["text" .= t, "functionCall" .= fc]
  toJSON (GeminiPart Nothing Nothing) = object ["text" .= ("" :: Text)]

instance FromJSON GeminiPart where
  parseJSON = withObject "GeminiPart" $ \v ->
    GeminiPart <$> v .:? "text" <*> v .:? "functionCall"

data GeminiContent = GeminiContent
  { parts :: [GeminiPart]
  } deriving (Show)

instance ToJSON GeminiContent where
  toJSON (GeminiContent p) = object ["parts" .= p]

instance FromJSON GeminiContent where
  parseJSON = withObject "GeminiContent" $ \v ->
    GeminiContent <$> v .: "parts"

data GeminiRequest = GeminiRequest
  { contents :: [GeminiContent]
  , tools :: Maybe [GeminiTool]
  } deriving (Show)

instance ToJSON GeminiRequest where
  toJSON (GeminiRequest c Nothing) = object ["contents" .= c]
  toJSON (GeminiRequest c (Just t)) = object ["contents" .= c, "tools" .= t]

data GeminiCandidate = GeminiCandidate
  { candidateContent :: GeminiContent
  } deriving (Show)

instance FromJSON GeminiCandidate where
  parseJSON = withObject "GeminiCandidate" $ \v ->
    GeminiCandidate <$> v .: "content"

data GeminiResponse = GeminiResponse
  { candidates :: [GeminiCandidate]
  } deriving (Show)

instance FromJSON GeminiResponse where
  parseJSON = withObject "GeminiResponse" $ \v ->
    GeminiResponse <$> v .: "candidates"

-- | Create Gemini provider
geminiProvider :: LLMProvider
geminiProvider = LLMProvider
  { callLLM = callGemini
  }

-- Convert MCP tools to Gemini format
mcpToolsToGemini :: Types.MCPContext -> [GeminiTool]
mcpToolsToGemini ctx =
  [GeminiTool [GeminiFunctionDeclaration name desc schema | (name, desc, schema) <- Types.mcpTools ctx]]

callGemini :: LLMRequest -> IO (Either LLMError LLMResponse)
callGemini llmReq = do
  let apiKey' = case Types.apiKey llmReq of
        Nothing -> error "Gemini API key is required"
        Just k -> k
      model' = case Types.model llmReq of
        Nothing -> "gemini-1.5-flash"
        Just m -> m
      -- Build contents from history + current prompt
      historyContents = map (\msg -> GeminiContent [GeminiPart (Just $ Types.messageContent msg) Nothing]) (Types.history llmReq)
      allContents = historyContents ++ [GeminiContent [GeminiPart (Just $ Types.prompt llmReq) Nothing]]
      -- Convert MCP tools to Gemini format
      tools' = case Types.mcpContext llmReq of
        Nothing -> Nothing
        Just ctx -> if null (Types.mcpTools ctx) then Nothing else Just $ mcpToolsToGemini ctx

  if Types.streaming llmReq
    then callGeminiStream apiKey' model' allContents tools'
    else callGeminiNonStream apiKey' model' allContents tools'

-- | Build Gemini request
buildGeminiRequest :: Text -> Text -> [GeminiContent] -> Maybe [GeminiTool] -> Bool -> IO Request
buildGeminiRequest apiKey' model' contents' tools' stream' = do
  let requestBody = GeminiRequest
        { contents = contents'
        , tools = tools'
        }
      endpoint = if stream' then ":streamGenerateContent?alt=sse" else ":generateContent"
      url = "POST https://generativelanguage.googleapis.com/v1beta/models/"
            <> T.unpack model' <> endpoint
  request' <- parseRequest url
  return $ setRequestBodyJSON requestBody
         $ setRequestHeader "x-goog-api-key" [TE.encodeUtf8 apiKey']
         $ request'

callGeminiNonStream :: Text -> Text -> [GeminiContent] -> Maybe [GeminiTool] -> IO (Either LLMError LLMResponse)
callGeminiNonStream apiKey' model' contents' tools' = do
  result <- try $ do
    request <- buildGeminiRequest apiKey' model' contents' tools' False
    response <- httpLBS request
    let body = getResponseBody response
    case eitherDecode body of
      Left err -> return $ Left $ ParseError err
      Right geminiResp ->
        case candidates geminiResp of
          [] -> return $ Left $ APIError "No response from Gemini"
          (candidate:_) -> do
            let (textContent', toolCalls') = extractContent (parts $ candidateContent candidate)
            return $ Right $ LLMResponse textContent' toolCalls'

  case result of
    Left (e :: SomeException) -> return $ Left $ NetworkError (show e)
    Right res -> return res
  where
    extractContent :: [GeminiPart] -> (Text, Maybe [Types.ToolCall])
    extractContent partsList =
      let texts = [t | GeminiPart (Just t) _ <- partsList]
          functionCalls = [fc | GeminiPart _ (Just fc) <- partsList]
          textContent' = T.intercalate "\n" texts
          toolCalls' = if null functionCalls
                       then Nothing
                       else Just $ zipWith convertFunctionCall [1..] functionCalls
      in (textContent', toolCalls')

    convertFunctionCall :: Int -> GeminiFunctionCall -> Types.ToolCall
    convertFunctionCall idx fc = Types.ToolCall
      (T.pack $ "call_" <> show idx)  -- Gemini doesn't provide IDs, so we generate them
      (fcName fc)
      (T.pack $ show $ fcArgs fc)  -- Convert Value to JSON string

callGeminiStream :: Text -> Text -> [GeminiContent] -> Maybe [GeminiTool] -> IO (Either LLMError LLMResponse)
callGeminiStream apiKey' model' contents' tools' = do
  result <- try $ do
    request <- buildGeminiRequest apiKey' model' contents' tools' True
    withResponse request $ \response -> do
      firstChunkRef <- newIORef True
      runConduit $ responseBody response
        .| CC.linesUnboundedAscii
        .| CL.mapMaybe extractData
        .| CL.mapM_ (processGeminiChunk firstChunkRef)
      return ()

  case result of
    Left (e :: SomeException) -> return $ Left $ NetworkError (show e)
    Right () -> return $ Right $ LLMResponse "" Nothing
  where
    extractData line
      | "data: " `BS.isPrefixOf` line = Just $ BS.drop 6 line
      | otherwise = Nothing

processGeminiChunk :: IORef Bool -> BS.ByteString -> IO ()
processGeminiChunk firstChunkRef chunk
  | BS.null chunk = return ()
  | otherwise = case eitherDecode (LBS.fromStrict chunk) of
      Right (geminiResp :: GeminiResponse) ->
        case candidates geminiResp of
          (candidate:_) ->
            case parts (candidateContent candidate) of
              (part:_) ->
                case partText part of
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
          [] -> return ()
      Left _ -> return ()
