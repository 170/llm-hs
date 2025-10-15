{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module LLM.Types
  ( Provider(..)
  , LLMRequest(..)
  , LLMResponse(..)
  , LLMError(..)
  , Message(..)
  , ConversationHistory
  ) where

import Data.Text (Text)
import GHC.Generics (Generic)

-- | LLM providers
data Provider
  = OpenAI
  | Claude
  | Ollama
  | Gemini
  deriving (Show, Eq, Generic)

-- | Request to LLM
data LLMRequest = LLMRequest
  { prompt :: Text
  , model :: Maybe Text
  , apiKey :: Maybe Text
  , baseUrl :: Maybe Text
  , streaming :: Bool
  , history :: ConversationHistory
  } deriving (Show, Eq, Generic)

-- | Response from LLM
data LLMResponse = LLMResponse
  { content :: Text
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
