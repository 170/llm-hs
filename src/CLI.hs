{-# LANGUAGE OverloadedStrings #-}

module CLI
  ( Options(..)
  , parseOptions
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import Core.Types (Provider(..), ColorMode(..))

data Options = Options
  { provider :: Maybe Provider
  , modelName :: Maybe Text
  , apiKeyOpt :: Maybe Text
  , baseUrlOpt :: Maybe Text
  , streamOpt :: Maybe Bool
  , colorOpt :: Maybe ColorMode
  , systemPrompt :: Maybe Text
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
  <*> flag Nothing (Just True)
      (  long "stream"
      <> short 's'
      <> help "Enable streaming output (real-time)"
      )
  <*> colorModeParser
  <*> optional (option (T.pack <$> str)
      (  long "system"
      <> metavar "PROMPT"
      <> help "Custom system prompt"
      ))

parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (optionsParser <**> helper)
      (  fullDesc
      <> progDesc "Call LLM APIs with input from stdin"
      <> header "llm-hs - A CLI tool for calling various LLM APIs"
      )
