{-# LANGUAGE OverloadedStrings #-}

module App.Interactive
  ( runInteractive
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (stderr, hFlush, stdout)
import System.Console.Haskeline
import System.Console.ANSI (SGR(..), Color(..), ColorIntensity(..), ConsoleLayer(..), setSGRCode)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless, when)
import qualified Data.Maybe
import Core.Types (LLMRequest(..), LLMResponse(..), Message(..), ConversationHistory, MCPContext(..), Provider(..), ColorMode(..))
import CLI (Options(..))
import Integration.MCP (MCPClient)
import UI.Spinner (startSpinner, stopSpinner)
import qualified UI.Color as Color
import qualified Core.Types as Types
import App.MCPHandler (executeToolCall)
import App.Utils (getApiKey, createSystemMessage, callProvider)

runInteractive :: Options -> [MCPClient] -> Maybe MCPContext -> IO ()
runInteractive opts mcpClients mcpCtx = do
  let prov = case provider opts of
        Just p -> p
        Nothing -> error "Provider must be set"  -- Should never happen after merge
      colorMode = Data.Maybe.fromMaybe AutoColor (colorOpt opts)

  -- Print header with colors
  headerText <- Color.infoColor colorMode "=== Interactive Mode ==="
  TIO.putStrLn headerText

  providerText <- Color.infoColor colorMode $ "Provider: " <> T.pack (show prov)
  TIO.putStrLn providerText

  let defaultModel' = case prov of
        OpenAI -> "gpt-4o-mini"
        Claude -> "claude-3-5-sonnet-20241022"
        Ollama -> "llama3.2"
        Gemini -> "gemini-1.5-flash"
  case modelName opts of
    Just model' -> do
      modelText <- Color.infoColor colorMode $ "Model: " <> model'
      TIO.putStrLn modelText
    Nothing -> do
      modelText <- Color.infoColor colorMode $ "Model: " <> defaultModel' <> " (default)"
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

  -- Check if color should be used for input
  useColor <- Color.shouldUseColor colorMode

  -- Start conversation loop with haskeline
  runInputT defaultSettings $ conversationLoop opts apiKeyValue mcpClients mcpCtx colorMode useColor [systemMsg]

conversationLoop :: Options -> Maybe T.Text -> [MCPClient] -> Maybe MCPContext -> ColorMode -> Bool -> ConversationHistory -> InputT IO ()
conversationLoop opts apiKey' mcpClients mcpCtx colorMode useColor history' = do
  -- Create colored prompt with ANSI codes for both prompt and input text
  let greenColor = setSGRCode [SetColor Foreground Vivid Green]
      resetColor = setSGRCode [Reset]
      coloredPrompt = if useColor
                      then greenColor ++ "> " ++ resetColor ++ greenColor
                      else "> "

  -- Read user input with line editing support
  minput <- getInputLine coloredPrompt

  -- Reset color after input
  when useColor $ liftIO $ putStr resetColor

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
                , apiKey = apiKey'
                , baseUrl = baseUrlOpt opts
                , streaming = Data.Maybe.fromMaybe False (streamOpt opts)
                , history = history'  -- Pass conversation history to API
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
              conversationLoop opts apiKey' mcpClients mcpCtx colorMode useColor history'
            Right response -> do
              -- Check if there are tool calls
              case Types.toolCalls response of
                Just calls | not (null calls) -> do
                  -- Show assistant's response/thinking if present
                  let assistantMsg = content response
                  unless (T.null assistantMsg) $
                    liftIO $ TIO.putStrLn assistantMsg

                  -- Execute tool calls
                  liftIO $ TIO.putStrLn ""
                  toolResults <- liftIO $ mapM (executeToolCall mcpClients colorMode) calls
                  liftIO $ TIO.putStrLn ""

                  -- Build tool result message
                  let toolResultText = T.intercalate "\n\n"
                        [ "Tool: " <> Types.toolName tc <> "\nResult: " <> res
                        | (tc, res) <- zip calls toolResults
                        ]

                  -- Add user message, assistant tool call, and tool results to history
                  let updatedHistory = history' ++ [Message "user" userInput]

                  -- Make another API call with tool results
                  -- Don't include mcpContext to avoid sending tool definitions again
                  -- Force streaming for better responsiveness
                  let request' = LLMRequest
                        { prompt = "Based on the tool results:\n" <> toolResultText
                        , model = modelName opts
                        , apiKey = apiKey'
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
                      conversationLoop opts apiKey' mcpClients mcpCtx colorMode useColor updatedHistory
                    Right response' -> do
                      -- Streaming mode outputs directly, so content will be empty
                      -- Just add newline and continue
                      liftIO $ TIO.putStrLn ""

                      -- For history, only add assistant message if content is not empty
                      -- (streaming mode returns empty content since it was already output)
                      let assistantContent = content response'
                          finalHistory = if T.null assistantContent
                                        then updatedHistory ++ [Message "assistant" "[Response from tool results]"]
                                        else updatedHistory ++ [Message "assistant" assistantContent]
                      conversationLoop opts apiKey' mcpClients mcpCtx colorMode useColor finalHistory

                _ -> do
                  -- No tool calls, normal response
                  let assistantMsg = content response

                  if Data.Maybe.fromMaybe False (streamOpt opts)
                    then liftIO $ TIO.putStrLn ""  -- Add newline after streaming
                    else liftIO $ TIO.putStrLn assistantMsg

                  liftIO $ hFlush stdout
                  liftIO $ TIO.putStrLn ""

                  -- Add both user and assistant messages to history
                  -- In streaming mode, content is empty so use a placeholder
                  let assistantContent = if T.null assistantMsg
                                        then "[Streamed response]"
                                        else assistantMsg
                      updatedHistory = history' ++ [Message "user" userInput, Message "assistant" assistantContent]

                  -- Continue conversation
                  conversationLoop opts apiKey' mcpClients mcpCtx colorMode useColor updatedHistory
