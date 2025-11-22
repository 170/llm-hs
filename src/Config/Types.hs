{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Config.Types
  ( Config(..)
  , MCPServer(..)
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?), (.!=), withObject)
import Data.Text (Text)
import Data.Map.Strict (Map)
import GHC.Generics (Generic)
import Core.Types (Provider(..), ColorMode(..))

-- MCP Server configuration
data MCPServer = MCPServer
  { serverName :: Text
  , serverCommand :: Text
  , serverArgs :: [Text]
  , serverEnv :: Maybe (Map Text Text)
  } deriving (Show, Eq, Generic)

instance FromJSON MCPServer where
  parseJSON = withObject "MCPServer" $ \v ->
    MCPServer
      <$> v .: "name"
      <*> v .: "command"
      <*> v .:? "args" .!= []
      <*> v .:? "env"

instance ToJSON MCPServer where
  toJSON (MCPServer name cmd args env) =
    object ["name" .= name, "command" .= cmd, "args" .= args, "env" .= env]

-- Main configuration
data Config = Config
  { defaultProvider :: Maybe Provider
  , defaultModel :: Maybe Text
  , openaiApiKey :: Maybe Text
  , claudeApiKey :: Maybe Text
  , geminiApiKey :: Maybe Text
  , defaultBaseUrl :: Maybe Text
  , defaultStream :: Maybe Bool
  , defaultColor :: Maybe ColorMode
  , mcpServers :: [MCPServer]
  } deriving (Show, Eq, Generic)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> do
    provider <- v .:? "provider"
    let parsedProvider = provider >>= parseProvider
    color <- v .:? "color"
    let parsedColor = color >>= parseColor
    Config parsedProvider
      <$> v .:? "model"
      <*> v .:? "openaiApiKey"
      <*> v .:? "claudeApiKey"
      <*> v .:? "geminiApiKey"
      <*> v .:? "baseUrl"
      <*> v .:? "stream"
      <*> pure parsedColor
      <*> v .:? "mcpServers" .!= []
    where
      parseProvider :: Text -> Maybe Provider
      parseProvider "openai" = Just OpenAI
      parseProvider "claude" = Just Claude
      parseProvider "ollama" = Just Ollama
      parseProvider "gemini" = Just Gemini
      parseProvider _ = Nothing

      parseColor :: Text -> Maybe ColorMode
      parseColor "auto" = Just AutoColor
      parseColor "always" = Just AlwaysColor
      parseColor "never" = Just NoColor
      parseColor _ = Nothing

instance ToJSON Config where
  toJSON (Config prov model openaiKey claudeKey geminiKey baseUrl stream color servers) =
    object
      [ "provider" .= fmap providerToText prov
      , "model" .= model
      , "openaiApiKey" .= openaiKey
      , "claudeApiKey" .= claudeKey
      , "geminiApiKey" .= geminiKey
      , "baseUrl" .= baseUrl
      , "stream" .= stream
      , "color" .= fmap colorToText color
      , "mcpServers" .= servers
      ]
    where
      providerToText :: Provider -> Text
      providerToText OpenAI = "openai"
      providerToText Claude = "claude"
      providerToText Ollama = "ollama"
      providerToText Gemini = "gemini"

      colorToText :: ColorMode -> Text
      colorToText AutoColor = "auto"
      colorToText AlwaysColor = "always"
      colorToText NoColor = "never"
