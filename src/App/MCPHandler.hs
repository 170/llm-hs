{-# LANGUAGE OverloadedStrings #-}

module App.MCPHandler
  ( startMCPServers
  , buildMCPContext
  , executeToolCall
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson
import qualified Data.Aeson.KeyMap
import qualified Data.Aeson.Key as Key
import Data.Foldable (toList)
import Data.Either (rights, lefts)
import qualified Data.Maybe
import System.IO (stderr)
import Core.Types (MCPContext(..), ColorMode)
import qualified Core.Types as Types
import Config (MCPServer)
import Integration.MCP (MCPClient, startMCPServer, listTools, callTool, MCPTool(..), toolName, toolDescription, toolInputSchema)
import qualified UI.Color as Color
import qualified UI.Format as Format

-- | Start all MCP servers
startMCPServers :: [MCPServer] -> IO [MCPClient]
startMCPServers servers = do
  results <- mapM startMCPServer servers
  let clients = rights results
  let errors = lefts results

  -- Print errors if any
  mapM_ (\err -> TIO.hPutStrLn stderr $ "MCP Error: " <> T.pack err) errors

  return clients

-- | Build MCP context from clients
buildMCPContext :: [MCPClient] -> IO (Maybe MCPContext)
buildMCPContext [] = return Nothing
buildMCPContext clients = do
  -- Collect tools from all clients
  allTools <- concat <$> mapM getClientTools clients
  return $ Just $ MCPContext
    { mcpTools = allTools
    , mcpResources = []  -- Resources not implemented yet
    }
  where
    getClientTools client = do
      result <- listTools client
      case result of
        Left _ -> return []
        Right tools -> return
          [ (toolName tool, Data.Maybe.fromMaybe "" (toolDescription tool), toolInputSchema tool)
          | tool <- tools
          ]

-- | Execute a tool call via MCP
executeToolCall :: [MCPClient] -> ColorMode -> Types.ToolCall -> IO T.Text
executeToolCall clients colorMode tc = do
  -- Build content lines for the box
  let title = "Tool: " <> Types.toolName tc
  argsLines <- case Data.Aeson.eitherDecode (LBS.fromStrict $ TE.encodeUtf8 $ Types.toolArguments tc) of
    Right (Data.Aeson.Object obj) -> do
      let args = [ Key.toText k <> ": " <> formatValue v | (k, v) <- Data.Aeson.KeyMap.toList obj ]
      return args
    _ -> return []

  -- Draw box around tool execution info
  let boxLines = Format.drawBox title argsLines

  -- Color and print each line
  mapM_ (\boxLine -> do
    coloredPrefix <- Color.toolColor colorMode (Format.boxPrefix boxLine)
    coloredSuffix <- Color.toolColor colorMode (Format.boxSuffix boxLine)
    let line = coloredPrefix <> Format.boxContent boxLine <> coloredSuffix
    TIO.putStrLn line
    ) boxLines

  -- Try each client until one succeeds
  results <- mapM (\client -> callTool client (Types.toolName tc) (decodeArgs $ Types.toolArguments tc)) clients

  case rights results of
    (result:_) -> do
      -- Parse the result to extract relevant text
      case result of
        Data.Aeson.Object obj ->
          case Data.Aeson.KeyMap.lookup "content" obj of
            Just (Data.Aeson.Array arr) ->
              let textParts = [t | Data.Aeson.Object o <- toList arr, Just (Data.Aeson.String t) <- [Data.Aeson.KeyMap.lookup "text" o]]
              in return $ T.intercalate "\n" textParts
            _ -> return $ T.pack $ show result
        _ -> return $ T.pack $ show result
    [] -> return "Error: Tool call failed"
  where
    decodeArgs :: T.Text -> Data.Aeson.Value
    decodeArgs argsText = case Data.Aeson.eitherDecode (LBS.fromStrict $ TE.encodeUtf8 argsText) of
      Right val -> val
      Left _ -> Data.Aeson.object []

    formatValue :: Data.Aeson.Value -> T.Text
    formatValue (Data.Aeson.String s) = "\"" <> s <> "\""
    formatValue (Data.Aeson.Number n) = T.pack (show n)
    formatValue (Data.Aeson.Bool b) = T.pack (show b)
    formatValue Data.Aeson.Null = "null"
    formatValue v = T.pack (show v)
