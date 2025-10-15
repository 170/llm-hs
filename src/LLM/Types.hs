{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LLM.Types
  ( Provider(..)
  , LLMRequest(..)
  , LLMResponse(..)
  , LLMError(..)
  , Message(..)
  , ConversationHistory
  , MCPContext(..)
  , ToolCall(..)
  , ColorMode(..)
  ) where

import Data.Aeson (Value)
import Data.Text (Text)
import GHC.Generics (Generic)

-- | LLM providers
data Provider
  = OpenAI
  | Claude
  | Ollama
  | Gemini
  deriving (Show, Eq, Generic)

-- | Color mode for output
data ColorMode
  = NoColor      -- ^ Disable colored output
  | AutoColor    -- ^ Automatically detect if terminal supports color
  | AlwaysColor  -- ^ Always use colored output
  deriving (Show, Eq, Generic)

-- | Request to LLM
data LLMRequest = LLMRequest
  { prompt :: Text
  , model :: Maybe Text
  , apiKey :: Maybe Text
  , baseUrl :: Maybe Text
  , streaming :: Bool
  , history :: ConversationHistory
  , mcpContext :: Maybe MCPContext
  } deriving (Show, Eq, Generic)

-- | Tool call information
data ToolCall = ToolCall
  { toolCallId :: Text
  , toolName :: Text
  , toolArguments :: Text  -- JSON string
  } deriving (Show, Eq, Generic)

-- | Response from LLM
data LLMResponse = LLMResponse
  { content :: Text
  , toolCalls :: Maybe [ToolCall]
  } deriving (Show, Eq, Generic)

-- | Error types
data LLMError
  = NetworkError String
  | APIError String
  | ParseError String
  | ConfigError String
  deriving (Show, Eq, Generic)

-- | Message in a conversation
data Message = Message
  { role :: Text
  , messageContent :: Text
  } deriving (Show, Eq, Generic)

-- | Conversation history
type ConversationHistory = [Message]

-- | MCP Context (tools and resources available)
data MCPContext = MCPContext
  { mcpTools :: [(Text, Text, Value)]  -- (name, description, schema)
  , mcpResources :: [(Text, Text)]     -- (uri, description)
  } deriving (Show, Eq, Generic)
