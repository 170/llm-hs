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
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.Aeson.Key as Key
import Data.Foldable (toList)
import Data.Either (rights, lefts)
import qualified Data.Maybe as Maybe
import System.IO (stderr)
import Core.Types (MCPContext(..), ColorMode)
import qualified Core.Types as Types
import Config.Types (MCPServer)
import Integration.MCP (MCPClient, startMCPServer, listTools, callTool, MCPTool(..))
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
          [ (toolName tool, Maybe.fromMaybe "" (toolDescription tool), toolInputSchema tool)
          | tool <- tools
          ]

-- | Execute a tool call via MCP
executeToolCall :: [MCPClient] -> ColorMode -> Types.ToolCall -> IO T.Text
executeToolCall clients colorMode tc = do
  printToolCallInfo colorMode tc
  result <- callToolWithClients clients tc
  return $ extractTextFromResult result

printToolCallInfo :: ColorMode -> Types.ToolCall -> IO ()
printToolCallInfo colorMode tc = do
  let title = "Tool: " <> Types.toolName tc
  argsLines <- parseArgumentLines (Types.toolArguments tc)
  let boxLines = Format.drawBox title argsLines
  mapM_ (printColoredBoxLine colorMode) boxLines

parseArgumentLines :: T.Text -> IO [T.Text]
parseArgumentLines argsText =
  case Aeson.eitherDecode (LBS.fromStrict $ TE.encodeUtf8 argsText) of
    Right (Aeson.Object obj) ->
      return [ Key.toText k <> ": " <> formatValue v | (k, v) <- KeyMap.toList obj ]
    _ -> return ["Invalid JSON arguments"]

printColoredBoxLine :: ColorMode -> Format.BoxLine -> IO ()
printColoredBoxLine colorMode boxLine = do
  coloredPrefix <- Color.toolColor colorMode (Format.boxPrefix boxLine)
  coloredSuffix <- Color.toolColor colorMode (Format.boxSuffix boxLine)
  let line = coloredPrefix <> Format.boxContent boxLine <> coloredSuffix
  TIO.putStrLn line

callToolWithClients :: [MCPClient] -> Types.ToolCall -> IO (Either T.Text Aeson.Value)
callToolWithClients clients tc = do
  let toolArgs = decodeArgs $ Types.toolArguments tc
  results <- mapM (\client -> callTool client (Types.toolName tc) toolArgs) clients
  case rights results of
    (result:_) -> return $ Right result
    [] -> return $ Left "Error: Tool call failed or tool not found"

decodeArgs :: T.Text -> Aeson.Value
decodeArgs argsText = case Aeson.eitherDecode (LBS.fromStrict $ TE.encodeUtf8 argsText) of
  Right val -> val
  Left _ -> Aeson.object []

extractTextFromResult :: Either T.Text Aeson.Value -> T.Text
extractTextFromResult (Left err) = err
extractTextFromResult (Right val) = extractTextFromValue val

extractTextFromValue :: Aeson.Value -> T.Text
extractTextFromValue (Aeson.Object obj) =
  case KeyMap.lookup "content" obj of
    Just (Aeson.Array arr) ->
      let textParts = [t | Aeson.Object o <- toList arr, Just (Aeson.String t) <- [KeyMap.lookup "text" o]]
      in T.intercalate "\n" textParts
    _ -> T.pack $ show (Aeson.Object obj)
extractTextFromValue v = T.pack $ show v

formatValue :: Aeson.Value -> T.Text
formatValue (Aeson.String s) = "\"" <> s <> "\""
formatValue (Aeson.Number n) = T.pack (show n)
formatValue (Aeson.Bool b) = T.pack (show b)
formatValue Aeson.Null = "null"
formatValue v = T.pack (show v)
