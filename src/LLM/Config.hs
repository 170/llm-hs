{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLM.Config
  ( Config(..)
  , MCPServer(..)
  , loadConfig
  , getConfigPath
  ) where

import Data.Aeson (FromJSON(..), ToJSON(..), object, (.=), (.:), (.:?), (.!=), withObject, eitherDecode)
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import System.Directory (doesFileExist, getHomeDirectory)
import System.FilePath ((</>))
import Control.Exception (try, SomeException)
import LLM.Types (Provider(..))

-- MCP Server configuration
data MCPServer = MCPServer
  { serverName :: Text
  , serverCommand :: Text
  , serverArgs :: [Text]
  , serverEnv :: Maybe (Map Text Text)
  } deriving (Show)

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
  , defaultApiKey :: Maybe Text
  , defaultBaseUrl :: Maybe Text
  , defaultStream :: Maybe Bool
  , mcpServers :: [MCPServer]
  } deriving (Show)

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> do
    provider <- v .:? "provider"
    let parsedProvider = provider >>= parseProvider
    Config
      <$> pure parsedProvider
      <*> v .:? "model"
      <*> v .:? "apiKey"
      <*> v .:? "baseUrl"
      <*> v .:? "stream"
      <*> v .:? "mcpServers" .!= []
    where
      parseProvider :: Text -> Maybe Provider
      parseProvider "openai" = Just OpenAI
      parseProvider "claude" = Just Claude
      parseProvider "ollama" = Just Ollama
      parseProvider "gemini" = Just Gemini
      parseProvider _ = Nothing

instance ToJSON Config where
  toJSON (Config prov model key baseUrl stream servers) =
    object
      [ "provider" .= fmap providerToText prov
      , "model" .= model
      , "apiKey" .= key
      , "baseUrl" .= baseUrl
      , "stream" .= stream
      , "mcpServers" .= servers
      ]
    where
      providerToText :: Provider -> Text
      providerToText OpenAI = "openai"
      providerToText Claude = "claude"
      providerToText Ollama = "ollama"
      providerToText Gemini = "gemini"

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
