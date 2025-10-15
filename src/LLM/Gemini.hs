{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLM.Gemini (callGemini) where

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
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import Network.HTTP.Simple
import Network.HTTP.Conduit (responseBody)
import qualified LLM.Types as Types
import LLM.Types (LLMRequest, LLMResponse(..), LLMError(..))
import LLM.Spinner (stopSpinner)

data GeminiPart = GeminiPart
  { partText :: Text
  } deriving (Show)

instance ToJSON GeminiPart where
  toJSON (GeminiPart t) = object ["text" .= t]

instance FromJSON GeminiPart where
  parseJSON = withObject "GeminiPart" $ \v ->
    GeminiPart <$> v .: "text"

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
  } deriving (Show)

instance ToJSON GeminiRequest where
  toJSON (GeminiRequest c) = object ["contents" .= c]

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

callGemini :: LLMRequest -> IO (Either LLMError LLMResponse)
callGemini llmReq = do
  let apiKey' = case Types.apiKey llmReq of
        Nothing -> error "Gemini API key is required"
        Just k -> k
      model' = case Types.model llmReq of
        Nothing -> "gemini-1.5-flash"
        Just m -> m
      -- Build contents from history + current prompt
      -- Gemini uses "user" and "model" roles
      historyContents = map (\msg ->
        let roleText = if Types.role msg == "assistant" then "model" else Types.role msg
        in GeminiContent [GeminiPart (Types.messageContent msg)]) (Types.history llmReq)
      allContents = historyContents ++ [GeminiContent [GeminiPart (Types.prompt llmReq)]]

  if Types.streaming llmReq
    then callGeminiStream apiKey' model' allContents
    else callGeminiNonStream apiKey' model' allContents

callGeminiNonStream :: Text -> Text -> [GeminiContent] -> IO (Either LLMError LLMResponse)
callGeminiNonStream apiKey' model' contents' = do
  result <- try $ do
    let requestBody = GeminiRequest
          { contents = contents'
          }
        url = "POST https://generativelanguage.googleapis.com/v1beta/models/"
              <> T.unpack model' <> ":generateContent"

    request' <- parseRequest url
    let request = setRequestBodyJSON requestBody
                $ setRequestHeader "x-goog-api-key" [TE.encodeUtf8 apiKey']
                $ request'

    response <- httpJSON request
    return $ getResponseBody response :: IO GeminiResponse

  case result of
    Left (e :: SomeException) -> return $ Left $ NetworkError (show e)
    Right geminiResp ->
      case candidates geminiResp of
        [] -> return $ Left $ APIError "No response from Gemini"
        (candidate:_) ->
          case parts (candidateContent candidate) of
            [] -> return $ Left $ APIError "No content in Gemini response"
            (part:_) -> return $ Right $ LLMResponse (partText part) Nothing

callGeminiStream :: Text -> Text -> [GeminiContent] -> IO (Either LLMError LLMResponse)
callGeminiStream apiKey' model' contents' = do
  result <- try $ do
    let requestBody = GeminiRequest
          { contents = contents'
          }
        url = "POST https://generativelanguage.googleapis.com/v1beta/models/"
              <> T.unpack model' <> ":streamGenerateContent?alt=sse"

    request' <- parseRequest url
    let request = setRequestBodyJSON requestBody
                $ setRequestHeader "x-goog-api-key" [TE.encodeUtf8 apiKey']
                $ request'

    withResponse request $ \response -> do
      firstChunkRef <- newIORef True
      runConduit $ responseBody response .| CC.linesUnboundedAscii .| parseGeminiSSE .| CL.mapM_ (processGeminiChunk firstChunkRef)
      return ()

  case result of
    Left (e :: SomeException) -> return $ Left $ NetworkError (show e)
    Right () -> return $ Right $ LLMResponse "" Nothing

parseGeminiSSE :: Monad m => ConduitT BS.ByteString BS.ByteString m ()
parseGeminiSSE = CL.mapMaybe extractData
  where
    extractData line
      | "data: " `BS.isPrefixOf` line = Just $ BS.drop 6 line
      | otherwise = Nothing

processGeminiChunk :: IORef Bool -> BS.ByteString -> IO ()
processGeminiChunk firstChunkRef chunk
  | BS.null chunk = return ()
  | otherwise = case decode (LBS.fromStrict chunk) of
      Just (geminiResp :: GeminiResponse) ->
        case candidates geminiResp of
          (candidate:_) ->
            case parts (candidateContent candidate) of
              (part:_) -> do
                let txt = partText part
                -- Stop spinner on first chunk with content
                when (not $ T.null txt) $ do
                  isFirst <- readIORef firstChunkRef
                  when isFirst $ do
                    writeIORef firstChunkRef False
                    stopSpinner
                -- Output the content immediately
                TIO.putStr txt
                hFlush stdout
              [] -> return ()
          [] -> return ()
      Nothing -> return ()
