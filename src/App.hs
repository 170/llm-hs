module App
  ( runLLM
  ) where

import System.IO (stdin, hIsTerminalDevice)
import Control.Exception (bracket)
import CLI (Options(..))
import Config (loadConfig, Config(..))
import Integration.MCP (stopMCPServer)
import App.MCPHandler (startMCPServers, buildMCPContext)
import App.Interactive (runInteractive)
import App.Pipe (runPipe)

runLLM :: Options -> IO ()
runLLM opts = do
  -- Check if stdin is a terminal (interactive mode)
  isTerminal <- hIsTerminalDevice stdin

  if isTerminal
    then do
      -- Load config to get MCP servers (only for interactive mode)
      config <- loadConfig
      let servers = maybe [] mcpServers config

      -- Use bracket to ensure proper cleanup
      bracket
        (startMCPServers servers)
        (mapM_ stopMCPServer)
        (\clients -> do
          -- Build MCP context
          mcpCtx <- buildMCPContext clients
          runInteractive opts clients mcpCtx
        )
    else runPipe opts Nothing
