{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Config
  ( module Config.Types
  , loadConfig
  , getConfigPath
  , applyConfigToOptions
  ) where

import qualified Data.ByteString.Lazy as LBS
import Data.Aeson (eitherDecode)
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath ((</>))
import Control.Exception (try, SomeException)

import Control.Applicative ((<|>))
import qualified Data.Text

import Config.Types
import CLI (Options(..))
import Core.Types (Provider(..), ColorMode(..))

-- Get the configuration file path
getConfigPath :: IO FilePath
getConfigPath = do
  home <- getHomeDirectory
  return $ home </> ".llm-hs.json"

-- Load configuration from file
loadConfig :: IO (Maybe Config)
loadConfig = do
  configPath <- getConfigPath
  exists <- doesFileExist configPath
  if not exists
    then return Nothing
    else do
      result <- try $ LBS.readFile configPath
      case result of
        Left (_ :: SomeException) -> return Nothing
        Right content ->
          case eitherDecode content of
            Left _ -> return Nothing
            Right config -> return $ Just config

-- Merge configuration file settings with command-line options
-- Command-line options take precedence over config file
applyConfigToOptions :: Maybe Config -> Options -> Either String Options
applyConfigToOptions Nothing opts =
  case provider opts of
    Nothing -> Left "No provider specified. Please specify via --provider or in ~/.llm-hs.json"
    Just _ -> Right $ opts
      { streamOpt = streamOpt opts <|> Just False
      , colorOpt = colorOpt opts <|> Just AutoColor
      }
applyConfigToOptions (Just config) opts =
  let mergedProvider = provider opts <|> defaultProvider config
      mergedModel = modelName opts <|> defaultModel config
      mergedApiKey = apiKeyOpt opts <|> (mergedProvider >>= getApiKeyFromConfig config)
      mergedBaseUrl = baseUrlOpt opts <|> defaultBaseUrl config
      mergedStream = streamOpt opts <|> defaultStream config <|> Just False
      mergedColor = colorOpt opts <|> defaultColor config <|> Just AutoColor
  in case mergedProvider of
       Nothing -> Left "No provider specified. Please specify via --provider or in ~/.llm-hs.json"
       Just p -> Right $ opts
         { provider = Just p
         , modelName = mergedModel
         , apiKeyOpt = mergedApiKey
         , baseUrlOpt = mergedBaseUrl
         , streamOpt = mergedStream
         , colorOpt = mergedColor
         }

getApiKeyFromConfig :: Config -> Provider -> Maybe Data.Text.Text
getApiKeyFromConfig config OpenAI = openaiApiKey config
getApiKeyFromConfig config Claude = claudeApiKey config
getApiKeyFromConfig config Gemini = geminiApiKey config
getApiKeyFromConfig _ Ollama = Nothing
