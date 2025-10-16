{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( runLLM
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Aeson
import qualified Data.Aeson.KeyMap
import qualified Data.Aeson.Key as Key
import Data.Foldable (toList)
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import Data.Time.LocalTime (getCurrentTimeZone, utcToLocalTime)
import System.Environment (lookupEnv)
import System.IO (stderr, hFlush, stdout, stdin, hIsTerminalDevice)
import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Control.Exception (bracket)
import qualified LLM.Types as Types
import LLM.Types (LLMRequest(..), LLMResponse(..), LLMError(..), Message(..), ConversationHistory, MCPContext(..), Provider(..), ColorMode(..), LLMProvider(..))
import LLM.OpenAI (openAIProvider)
import LLM.Claude (claudeProvider)
import LLM.Ollama (ollamaProvider)
import LLM.Gemini (geminiProvider)
import LLM.CLI (Options(..))
import LLM.Spinner (startSpinner, stopSpinner)
import LLM.Config (loadConfig, MCPServer, Config(..))
import LLM.MCP (MCPClient, startMCPServer, stopMCPServer, listTools, callTool, MCPTool(..))
import qualified LLM.Color as Color
import qualified LLM.Format as Format

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
        (\clients -> mapM_ stopMCPServer clients)
        (\clients -> do
          -- Build MCP context
          mcpCtx <- buildMCPContext clients
          runInteractive opts clients mcpCtx
        )
    else runPipe opts Nothing

-- | Start all MCP servers
startMCPServers :: [MCPServer] -> IO [MCPClient]
startMCPServers servers = do
  results <- mapM startMCPServer servers
  let clients = [client | Right client <- results]
  let errors = [err | Left err <- results]

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
        Left err -> return []
        Right tools -> return
          [ (LLM.MCP.toolName tool, maybe "" id (LLM.MCP.toolDescription tool), LLM.MCP.toolInputSchema tool)
          | tool <- tools
          ]

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

-- | Create system message with current date and time
createSystemMessage :: IO Message
createSystemMessage = do
  now <- getCurrentTime
  tz <- getCurrentTimeZone
  let localTime = utcToLocalTime tz now
      dateTimeStr = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S %Z" localTime
      dateStr = formatTime defaultTimeLocale "%Y-%m-%d" localTime
      dayOfWeek = formatTime defaultTimeLocale "%A" localTime
      systemContent = T.unlines
        [ "Current date and time: " <> T.pack dateTimeStr
        , "Today's date: " <> T.pack dateStr <> " (" <> T.pack dayOfWeek <> ")"
        , ""
        , "IMPORTANT: When the user mentions relative dates like 'today', 'tomorrow', 'yesterday', etc., interpret them as specific dates:"
        , "- 'today' means " <> T.pack dateStr
        , "- Use this current date to calculate other relative dates"
        , "- When searching for news or events, use the specific date in your queries"
        ]
  return $ Message "system" systemContent

runInteractive :: Options -> [MCPClient] -> Maybe MCPContext -> IO ()
runInteractive opts mcpClients mcpCtx = do
  let prov = case provider opts of
        Just p -> p
        Nothing -> error "Provider must be set"  -- Should never happen after merge
      colorMode = maybe AutoColor id (colorOpt opts)

  -- Print header with colors
  headerText <- Color.infoColor colorMode "=== Interactive Mode ==="
  TIO.putStrLn headerText

  providerText <- Color.infoColor colorMode $ "Provider: " <> T.pack (show prov)
  TIO.putStrLn providerText

  let defaultModel = case prov of
        OpenAI -> "gpt-4o-mini"
        Claude -> "claude-3-5-sonnet-20241022"
        Ollama -> "llama3.2"
        Gemini -> "gemini-1.5-flash"
  case modelName opts of
    Just model -> do
      modelText <- Color.infoColor colorMode $ "Model: " <> model
      TIO.putStrLn modelText
    Nothing -> do
      modelText <- Color.infoColor colorMode $ "Model: " <> defaultModel <> " (default)"
      TIO.putStrLn modelText

  -- Show MCP status
  case mcpCtx of
    Nothing -> return ()
    Just ctx -> do
      let toolCount = length (mcpTools ctx)
      mcpText <- Color.infoColor colorMode $ "MCP Tools: " <> T.pack (show toolCount) <> " tools available"
      TIO.putStrLn mcpText

  instructionText1 <- Color.systemColor colorMode "Type your message and press Enter. Type 'exit' or 'quit' to end."
  TIO.putStrLn instructionText1

  instructionText2 <- Color.systemColor colorMode "Emacs keybindings supported (C-a, C-e, C-k, etc.)\n"
  TIO.putStrLn instructionText2

  -- Get API key
  apiKeyValue <- getApiKey opts

  -- Create initial system message with current time
  systemMsg <- createSystemMessage

  -- Start conversation loop with haskeline
  runInputT defaultSettings $ conversationLoop opts apiKeyValue mcpClients mcpCtx colorMode [systemMsg]

conversationLoop :: Options -> Maybe T.Text -> [MCPClient] -> Maybe MCPContext -> ColorMode -> ConversationHistory -> InputT IO ()
conversationLoop opts apiKey mcpClients mcpCtx colorMode history = do
  -- Read user input with line editing support
  minput <- getInputLine "> "

  case minput of
    Nothing -> outputStrLn "Goodbye!"  -- EOF (Ctrl-D)
    Just input -> do
      let userInput = T.pack input

      -- Check for exit commands
      if userInput `elem` ["exit", "quit", ":q"]
        then outputStrLn "Goodbye!"
        else do
          let request = LLMRequest
                { prompt = userInput
                , model = modelName opts
                , apiKey = apiKey
                , baseUrl = baseUrlOpt opts
                , streaming = maybe False id (streamOpt opts)
                , history = history  -- Pass conversation history to API
                , mcpContext = mcpCtx
                }

          -- Show spinner in interactive mode (always)
          liftIO startSpinner
          result <- liftIO $ callProvider opts request
          liftIO stopSpinner

          -- Handle the result and update history
          case result of
            Left err -> do
              errorText <- liftIO $ Color.errorColor colorMode $ "Error: " <> T.pack (show err)
              liftIO $ TIO.hPutStrLn stderr errorText
              conversationLoop opts apiKey mcpClients mcpCtx colorMode history
            Right response -> do
              -- Check if there are tool calls
              case Types.toolCalls response of
                Just calls | not (null calls) -> do
                  -- Show assistant's response/thinking if present
                  let assistantMsg = content response
                  when (not $ T.null assistantMsg) $ do
                    assistantText <- liftIO $ Color.assistantColor colorMode assistantMsg
                    liftIO $ TIO.putStrLn assistantText

                  -- Execute tool calls
                  liftIO $ TIO.putStrLn ""
                  toolResults <- liftIO $ mapM (executeToolCall mcpClients colorMode) calls
                  liftIO $ TIO.putStrLn ""

                  -- Build tool result message
                  let toolResultText = T.intercalate "\n\n"
                        [ "Tool: " <> Types.toolName tc <> "\nResult: " <> result
                        | (tc, result) <- zip calls toolResults
                        ]

                  -- Add user message, assistant tool call, and tool results to history
                  let updatedHistory = history ++ [Message "user" userInput]

                  -- Make another API call with tool results
                  -- Don't include mcpContext to avoid sending tool definitions again
                  -- Force streaming for better responsiveness
                  let request' = LLMRequest
                        { prompt = "Based on the tool results:\n" <> toolResultText
                        , model = modelName opts
                        , apiKey = apiKey
                        , baseUrl = baseUrlOpt opts
                        , streaming = True
                        , history = updatedHistory
                        , mcpContext = Nothing
                        }

                  liftIO startSpinner
                  result' <- liftIO $ callProvider opts request'
                  liftIO stopSpinner

                  case result' of
                    Left err -> do
                      errorText <- liftIO $ Color.errorColor colorMode $ "Error: " <> T.pack (show err)
                      liftIO $ TIO.hPutStrLn stderr errorText
                      conversationLoop opts apiKey mcpClients mcpCtx colorMode updatedHistory
                    Right response' -> do
                      -- Streaming mode outputs directly, so content will be empty
                      -- Just add newline and continue
                      liftIO $ TIO.putStrLn ""

                      -- For history, we'll use a placeholder since streaming already output
                      let finalHistory = updatedHistory ++ [Message "assistant" (content response')]
                      conversationLoop opts apiKey mcpClients mcpCtx colorMode finalHistory

                _ -> do
                  -- No tool calls, normal response
                  let assistantMsg = content response

                  if maybe False id (streamOpt opts)
                    then liftIO $ TIO.putStrLn ""  -- Add newline after streaming
                    else do
                      assistantText <- liftIO $ Color.assistantColor colorMode assistantMsg
                      liftIO $ TIO.putStrLn assistantText

                  liftIO $ hFlush stdout
                  liftIO $ TIO.putStrLn ""

                  -- Add both user and assistant messages to history
                  let updatedHistory = history ++ [Message "user" userInput, Message "assistant" assistantMsg]

                  -- Continue conversation
                  conversationLoop opts apiKey mcpClients mcpCtx colorMode updatedHistory


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

  case [r | Right r <- results] of
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

getApiKey :: Options -> IO (Maybe T.Text)
getApiKey opts = case apiKeyOpt opts of
  Just key -> return $ Just key
  Nothing -> do
    let prov = case provider opts of
          Just p -> p
          Nothing -> error "Provider must be set"
    envKey <- case prov of
      OpenAI -> lookupEnv "OPENAI_API_KEY"
      Claude -> lookupEnv "ANTHROPIC_API_KEY"
      Ollama -> return Nothing  -- Ollama doesn't need API key
      Gemini -> lookupEnv "GEMINI_API_KEY"
    return $ T.pack <$> envKey

-- | Get provider instance based on provider type
getProvider :: Provider -> LLMProvider
getProvider prov = case prov of
  OpenAI -> openAIProvider
  Claude -> claudeProvider
  Ollama -> ollamaProvider
  Gemini -> geminiProvider

callProvider :: Options -> LLMRequest -> IO (Either LLMError LLMResponse)
callProvider opts request =
  let prov = case provider opts of
        Just p -> p
        Nothing -> error "Provider must be set"
      providerInstance = getProvider prov
  in callLLM providerInstance request

handleResult :: Options -> Either LLMError LLMResponse -> IO ()
handleResult opts result = do
  let colorMode = maybe AutoColor id (colorOpt opts)
  case result of
    Left err -> do
      errorText <- Color.errorColor colorMode $ "Error: " <> T.pack (show err)
      TIO.hPutStrLn stderr errorText
    Right response -> do
      if maybe False id (streamOpt opts)
        then TIO.putStrLn ""  -- Add newline after streaming
        else do
          assistantText <- Color.assistantColor colorMode $ content response
          TIO.putStrLn assistantText
      hFlush stdout
