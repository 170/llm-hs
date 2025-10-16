{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLM.MCP
  ( MCPClient
  , MCPTool(..)
  , MCPResource(..)
  , startMCPServer
  , stopMCPServer
  , listTools
  , callTool
  , listResources
  , readResource
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Concurrent.Async (race)
import Control.Exception (try, SomeException, catch)
import Control.Monad (forever, void)
import Data.Aeson
import Data.Aeson.KeyMap (lookup)
import Prelude hiding (lookup)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as LBS8
import qualified Data.ByteString.Char8 as BS8
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import System.IO
import System.Process
import System.Environment (getEnvironment)
import LLM.Config (MCPServer(..))

-- | MCP Client handle
data MCPClient = MCPClient
  { clientProcess :: ProcessHandle
  , clientStdin :: Handle
  , clientStdout :: Handle
  , clientMsgId :: TVar Int
  , clientPendingResponses :: TVar (Map Int (TMVar Value))
  }

-- | MCP Tool definition
data MCPTool = MCPTool
  { toolName :: Text
  , toolDescription :: Maybe Text
  , toolInputSchema :: Value
  } deriving (Show, Generic)

instance FromJSON MCPTool where
  parseJSON = withObject "MCPTool" $ \v ->
    MCPTool
      <$> v .: "name"
      <*> v .:? "description"
      <*> v .: "inputSchema"

-- | MCP Resource definition
data MCPResource = MCPResource
  { resourceUri :: Text
  , resourceName :: Text
  , resourceDescription :: Maybe Text
  , resourceMimeType :: Maybe Text
  } deriving (Show, Generic)

instance FromJSON MCPResource where
  parseJSON = withObject "MCPResource" $ \v ->
    MCPResource
      <$> v .: "uri"
      <*> v .: "name"
      <*> v .:? "description"
      <*> v .:? "mimeType"

-- | JSON-RPC Request
data JSONRPCRequest = JSONRPCRequest
  { rpcJsonrpc :: Text
  , rpcId :: Int
  , rpcMethod :: Text
  , rpcParams :: Maybe Value
  } deriving (Show, Generic)

instance ToJSON JSONRPCRequest where
  toJSON (JSONRPCRequest jsonrpc msgId method params) =
    object
      [ "jsonrpc" .= jsonrpc
      , "id" .= msgId
      , "method" .= method
      , "params" .= params
      ]

-- | JSON-RPC Response
data JSONRPCResponse = JSONRPCResponse
  { respJsonrpc :: Text
  , respId :: Maybe Int
  , respResult :: Maybe Value
  , respError :: Maybe Value
  } deriving (Show, Generic)

instance FromJSON JSONRPCResponse where
  parseJSON = withObject "JSONRPCResponse" $ \v ->
    JSONRPCResponse
      <$> v .: "jsonrpc"
      <*> v .:? "id"
      <*> v .:? "result"
      <*> v .:? "error"

-- | Start an MCP server process
startMCPServer :: MCPServer -> IO (Either String MCPClient)
startMCPServer server = do
  let cmd = T.unpack $ serverCommand server
      args = map T.unpack $ serverArgs server

  -- Get current environment and merge with server-specific env vars
  currentEnv <- getEnvironment
  let additionalEnv = case serverEnv server of
        Nothing -> []
        Just envMap -> [(T.unpack k, T.unpack v) | (k, v) <- Map.toList envMap]
      mergedEnv = currentEnv ++ additionalEnv

  result <- try $ createProcess (proc cmd args)
    { std_in = CreatePipe
    , std_out = CreatePipe
    , std_err = NoStream  -- Suppress stderr output from MCP servers
    , env = Just mergedEnv
    }

  case result of
    Left (e :: SomeException) ->
      return $ Left $ "Failed to start MCP server: " ++ show e
    Right (Just hIn, Just hOut, Nothing, ph) -> do
      -- Set handles to line buffering
      hSetBuffering hIn LineBuffering
      hSetBuffering hOut LineBuffering

      -- Create client state
      msgIdVar <- newTVarIO 1
      pendingVar <- newTVarIO Map.empty

      let client = MCPClient
            { clientProcess = ph
            , clientStdin = hIn
            , clientStdout = hOut
            , clientMsgId = msgIdVar
            , clientPendingResponses = pendingVar
            }

      -- Start response handler thread
      void $ forkIO $ responseHandler client

      -- Send initialize request
      initResult <- sendInitialize client
      case initResult of
        Left err -> do
          stopMCPServer client
          return $ Left err
        Right _ -> return $ Right client

    Right _ -> return $ Left "Failed to create pipes for MCP server"

-- | Stop an MCP server
stopMCPServer :: MCPClient -> IO ()
stopMCPServer client = do
  hClose (clientStdin client)
  hClose (clientStdout client)
  terminateProcess (clientProcess client)

-- | Handle responses from MCP server
responseHandler :: MCPClient -> IO ()
responseHandler client = forever $ do
  catch (do
    line <- BS8.hGetLine (clientStdout client)
    let lazyLine = LBS.fromStrict line
    case eitherDecode lazyLine of
      Left _ -> return ()
      Right response -> do
        case respId response of
          Just msgId -> do
            -- Find and complete pending response
            mTMVar <- atomically $ do
              pending <- readTVar (clientPendingResponses client)
              case Map.lookup msgId pending of
                Nothing -> return Nothing
                Just tmvar -> do
                  modifyTVar' (clientPendingResponses client) (Map.delete msgId)
                  return $ Just tmvar

            case mTMVar of
              Just tmvar ->
                case respResult response of
                  Just result -> atomically $ putTMVar tmvar result
                  Nothing -> return () -- Error case
              Nothing -> return ()
          Nothing -> return () -- Notification
    ) (\(_ :: SomeException) -> return ())

-- | Send a JSON-RPC request and wait for response
sendRequest :: MCPClient -> Text -> Maybe Value -> IO (Either String Value)
sendRequest client method params = do
  -- Generate message ID
  msgId <- atomically $ do
    currentId <- readTVar (clientMsgId client)
    writeTVar (clientMsgId client) (currentId + 1)
    return currentId

  -- Create response TMVar
  responseTMVar <- newEmptyTMVarIO
  atomically $ modifyTVar' (clientPendingResponses client) (Map.insert msgId responseTMVar)

  -- Send request
  let request = JSONRPCRequest "2.0" msgId method params
  let requestJSON = encode request

  result <- try $ LBS8.hPutStrLn (clientStdin client) requestJSON
  hFlush (clientStdin client)
  case result of
    Left (e :: SomeException) ->
      return $ Left $ "Failed to send request: " ++ show e
    Right _ -> do
      -- Wait for response (with timeout of 10 seconds)
      raceResult <- race (threadDelay 10000000) (atomically $ readTMVar responseTMVar)
      case raceResult of
        Left _ -> do
          -- Timeout: clean up pending response
          atomically $ modifyTVar' (clientPendingResponses client) (Map.delete msgId)
          return $ Left "Timeout waiting for response"
        Right value -> return $ Right value

-- | Send initialize request
sendInitialize :: MCPClient -> IO (Either String Value)
sendInitialize client = do
  let params = object
        [ "protocolVersion" .= ("2024-11-05" :: Text)
        , "capabilities" .= object []
        , "clientInfo" .= object
            [ "name" .= ("llm-hs" :: Text)
            , "version" .= ("0.1.0" :: Text)
            ]
        ]
  result <- sendRequest client "initialize" (Just params)

  -- Send initialized notification after successful initialize
  case result of
    Right _ -> do
      sendNotification client "notifications/initialized" Nothing
      -- Give the server a moment to process the notification
      threadDelay 100000  -- 100ms
      return result
    Left _ -> return result

-- | Send a JSON-RPC notification (no response expected)
sendNotification :: MCPClient -> Text -> Maybe Value -> IO ()
sendNotification client method params = do
  let notification = case params of
        Nothing -> object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "method" .= method
          ]
        Just p -> object
          [ "jsonrpc" .= ("2.0" :: Text)
          , "method" .= method
          , "params" .= p
          ]
  let notificationJSON = encode notification
  void $ (try :: IO () -> IO (Either SomeException ())) $ LBS8.hPutStrLn (clientStdin client) notificationJSON
  hFlush (clientStdin client)

-- | List available tools from MCP server
listTools :: MCPClient -> IO (Either String [MCPTool])
listTools client = do
  result <- sendRequest client "tools/list" (Just $ object [])
  case result of
    Left err -> return $ Left err
    Right value ->
      case fromJSON value of
        Error err -> return $ Left $ "Failed to parse tools: " ++ err
        Success obj ->
          case obj of
            Object o ->
              case fromJSON <$> lookup "tools" o of
                Just (Success tools) -> return $ Right tools
                Just (Error err) -> return $ Left $ "Failed to parse tools array: " ++ err
                Nothing -> return $ Right []
            _ -> return $ Left "Invalid response format"

-- | Call a tool on the MCP server
callTool :: MCPClient -> Text -> Value -> IO (Either String Value)
callTool client toolName' args = do
  let params = object
        [ "name" .= toolName'
        , "arguments" .= args
        ]
  sendRequest client "tools/call" (Just params)

-- | List available resources from MCP server
listResources :: MCPClient -> IO (Either String [MCPResource])
listResources client = do
  result <- sendRequest client "resources/list" Nothing
  case result of
    Left err -> return $ Left err
    Right value ->
      case fromJSON value of
        Error err -> return $ Left $ "Failed to parse resources: " ++ err
        Success obj ->
          case obj of
            Object o ->
              case fromJSON <$> lookup "resources" o of
                Just (Success resources) -> return $ Right resources
                Just (Error err) -> return $ Left $ "Failed to parse resources array: " ++ err
                Nothing -> return $ Right []
            _ -> return $ Left "Invalid response format"

-- | Read a resource from the MCP server
readResource :: MCPClient -> Text -> IO (Either String Value)
readResource client uri = do
  let params = object ["uri" .= uri]
  sendRequest client "resources/read" (Just params)
