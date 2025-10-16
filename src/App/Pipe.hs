{-# LANGUAGE OverloadedStrings #-}

module App.Pipe
  ( runPipe
  ) where

import qualified Data.Text.IO as TIO
import Core.Types (LLMRequest(..), MCPContext)
import CLI (Options(..))
import App.Utils (getApiKey, createSystemMessage, callProvider, handleResult)

runPipe :: Options -> Maybe MCPContext -> IO ()
runPipe opts mcpCtx = do
  -- Read input from stdin
  input <- TIO.getContents

  -- Get API key
  apiKeyValue <- getApiKey opts

  -- Create system message with current time
  systemMsg <- createSystemMessage

  let request = LLMRequest
        { prompt = input
        , model = modelName opts
        , apiKey = apiKeyValue
        , baseUrl = baseUrlOpt opts
        , streaming = maybe False id (streamOpt opts)
        , history = [systemMsg]  -- Include system message with current time
        , mcpContext = mcpCtx
        }

  -- Call the appropriate provider
  result <- callProvider opts request

  -- Output the result
  handleResult opts result
