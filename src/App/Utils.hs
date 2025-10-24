{-# LANGUAGE OverloadedStrings #-}

module App.Utils
  ( getApiKey
  , createSystemMessage
  , callProvider
  , handleResult
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Maybe (fromMaybe)
import System.Environment (lookupEnv)
import System.IO (stderr, hFlush, stdout)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)
import Core.Types (LLMRequest(..), LLMResponse(..), LLMError(..), Message(..), Provider(..), ColorMode(..), LLMProvider(..))
import Providers.OpenAI (openAIProvider)
import Providers.Claude (claudeProvider)
import Providers.Ollama (ollamaProvider)
import Providers.Gemini (geminiProvider)
import CLI (Options(..))
import qualified UI.Color as Color

-- | Get API key from options or environment
getApiKey :: Options -> IO (Maybe T.Text)
getApiKey opts = case apiKeyOpt opts of
  Just key -> return $ Just key
  Nothing -> do
    maybe (return Nothing) getEnvForProvider (provider opts)
  where
    getEnvForProvider :: Provider -> IO (Maybe T.Text)
    getEnvForProvider OpenAI = fmap T.pack <$> lookupEnv "OPENAI_API_KEY"
    getEnvForProvider Claude = fmap T.pack <$> lookupEnv "ANTHROPIC_API_KEY"
    getEnvForProvider Ollama = return Nothing  -- Ollama doesn't need API key
    getEnvForProvider Gemini = fmap T.pack <$> lookupEnv "GEMINI_API_KEY"

-- | Create system message with current date and time
createSystemMessage :: Maybe T.Text -> IO Message
createSystemMessage customPrompt = do
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  let localTime = utcToLocalTime tz now
      dateTimeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" localTime
      dateStr = formatTime defaultTimeLocale "%Y-%m-%d" localTime
      dayOfWeek = formatTime defaultTimeLocale "%A" localTime
      defaultContent = T.unlines
        [ "Current date and time: " <> T.pack dateTimeStr
        , "Today's date: " <> T.pack dateStr <> " (" <> T.pack dayOfWeek <> ")"
        , ""
        , "IMPORTANT: When the user mentions relative dates like 'today', 'tomorrow', 'yesterday', etc., interpret them as specific dates:"
        , "- 'today' means " <> T.pack dateStr
        , "- Use this current date to calculate other relative dates"
        , "- When searching for news or events, use the specific date in your queries"
        ]
      systemContent = case customPrompt of
        Nothing -> defaultContent
        Just custom -> custom <> "\n\n" <> defaultContent
  return $ Message "system" systemContent

-- | Get provider instance based on provider type
getProvider :: Provider -> LLMProvider
getProvider prov = case prov of
  OpenAI -> openAIProvider
  Claude -> claudeProvider
  Ollama -> ollamaProvider
  Gemini -> geminiProvider

-- | Call the appropriate LLM provider
callProvider :: Options -> LLMRequest -> IO (Either LLMError LLMResponse)
callProvider opts request = case provider opts of
  Nothing -> return $ Left $ ConfigError "Provider must be set"
  Just prov -> callLLM (getProvider prov) request

-- | Handle and display the result from an LLM call
handleResult :: Options -> Either LLMError LLMResponse -> IO ()
handleResult opts result = do
  let colorMode = fromMaybe AutoColor (colorOpt opts)
  case result of
    Left err -> do
      errorText <- Color.errorColor colorMode $ "Error: " <> T.pack (show err)
      TIO.hPutStrLn stderr errorText
    Right response -> do
      if fromMaybe False (streamOpt opts)
        then TIO.putStrLn ""  -- Add newline after streaming
        else do
          assistantText <- Color.assistantColor colorMode $ content response
          TIO.putStrLn assistantText
      hFlush stdout
