{-# LANGUAGE OverloadedStrings #-}

module LLM.CLI
  ( Options(..)
  , parseOptions
  , mergeConfigWithOptions
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import LLM.Types (Provider(..), ColorMode(..))
import LLM.Config (Config(..))

data Options = Options
  { provider :: Maybe Provider
  , modelName :: Maybe Text
  , apiKeyOpt :: Maybe Text
  , baseUrlOpt :: Maybe Text
  , streamOpt :: Maybe Bool
  , colorOpt :: Maybe ColorMode
  } deriving (Show)

providerParser :: Parser (Maybe Provider)
providerParser = optional $ option (maybeReader parseProvider)
  (  long "provider"
  <> short 'p'
  <> metavar "PROVIDER"
  <> help "LLM provider (openai, claude, ollama, gemini)"
  )
  where
    parseProvider :: String -> Maybe Provider
    parseProvider "openai" = Just OpenAI
    parseProvider "claude" = Just Claude
    parseProvider "ollama" = Just Ollama
    parseProvider "gemini" = Just Gemini
    parseProvider _ = Nothing

colorModeParser :: Parser (Maybe ColorMode)
colorModeParser = optional $ option (maybeReader parseColorMode)
  (  long "color"
  <> short 'c'
  <> metavar "MODE"
  <> help "Color mode (auto, always, never) - default: auto"
  )
  where
    parseColorMode :: String -> Maybe ColorMode
    parseColorMode "auto" = Just AutoColor
    parseColorMode "always" = Just AlwaysColor
    parseColorMode "never" = Just NoColor
    parseColorMode _ = Nothing

optionsParser :: Parser Options
optionsParser = Options
  <$> providerParser
  <*> optional (option (T.pack <$> str)
      (  long "model"
      <> short 'm'
      <> metavar "MODEL"
      <> help "Model name (e.g., gpt-4o-mini, claude-3-5-sonnet-20241022)"
      ))
  <*> optional (option (T.pack <$> str)
      (  long "api-key"
      <> short 'k'
      <> metavar "API_KEY"
      <> help "API key for the provider"
      ))
  <*> optional (option (T.pack <$> str)
      (  long "base-url"
      <> short 'u'
      <> metavar "BASE_URL"
      <> help "Base URL for the provider (mainly for Ollama, default: localhost)"
      ))
  <*> optional (switch
      (  long "stream"
      <> short 's'
      <> help "Enable streaming output (real-time)"
      ))
  <*> colorModeParser

parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (optionsParser <**> helper)
      (  fullDesc
      <> progDesc "Call LLM APIs with input from stdin"
      <> header "llm-hs - A CLI tool for calling various LLM APIs"
      )

-- Merge configuration file settings with command-line options
-- Command-line options take precedence over config file
mergeConfigWithOptions :: Maybe Config -> Options -> Either String Options
mergeConfigWithOptions Nothing opts =
  case provider opts of
    Nothing -> Left "No provider specified. Please specify via --provider or in ~/.llm-hs.json"
    Just _ -> Right $ opts
      { streamOpt = Just (maybe False id (streamOpt opts))
      , colorOpt = Just (maybe AutoColor id (colorOpt opts))
      }
mergeConfigWithOptions (Just config) opts =
  let mergedProvider = case provider opts of
        Just p -> Just p
        Nothing -> defaultProvider config
      mergedModel = case modelName opts of
        Just m -> Just m
        Nothing -> defaultModel config
      mergedApiKey = case apiKeyOpt opts of
        Just k -> Just k
        Nothing -> defaultApiKey config
      mergedBaseUrl = case baseUrlOpt opts of
        Just u -> Just u
        Nothing -> defaultBaseUrl config
      mergedStream = case streamOpt opts of
        Just s -> Just s
        Nothing -> defaultStream config
      mergedColor = case colorOpt opts of
        Just c -> Just c
        Nothing -> defaultColor config
  in case mergedProvider of
       Nothing -> Left "No provider specified. Please specify via --provider or in ~/.llm-hs.json"
       Just p -> Right $ Options
         { provider = Just p
         , modelName = mergedModel
         , apiKeyOpt = mergedApiKey
         , baseUrlOpt = mergedBaseUrl
         , streamOpt = Just (maybe False id mergedStream)
         , colorOpt = Just (maybe AutoColor id mergedColor)
         }
