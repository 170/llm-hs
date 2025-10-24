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
import Core.Types (LLMRequest(..), LLMResponse(..), LLMError(..), Message(..), ConversationHistory, MCPContext(..), Provider(..), ColorMode(..))
import CLI (Options(..))
import Integration.MCP (MCPClient)
import UI.Spinner (startSpinner, stopSpinner)
import qualified UI.Color as Color
import qualified Core.Types as Types
import App.MCPHandler (executeToolCall)
import App.Utils (getApiKey, createSystemMessage, callProvider)

runInteractive :: Options -> [MCPClient] -> Maybe MCPContext -> IO ()
runInteractive opts mcpClients mcpCtx = case provider opts of
  Nothing -> do
    -- This should never happen after merge, but handle gracefully
    TIO.hPutStrLn stderr "Error: Provider must be set"
  Just prov -> do
    let colorMode = Data.Maybe.fromMaybe AutoColor (colorOpt opts)

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
    systemMsg <- createSystemMessage (systemPrompt opts)

    -- Check if color should be used for input
    useColor <- Color.shouldUseColor colorMode

    -- Start conversation loop with haskeline
    runInputT defaultSettings $ conversationLoop opts apiKeyValue mcpClients mcpCtx colorMode useColor [systemMsg]

conversationLoop :: Options -> Maybe T.Text -> [MCPClient] -> Maybe MCPContext -> ColorMode -> Bool -> ConversationHistory -> InputT IO ()
conversationLoop opts apiKey' mcpClients mcpCtx colorMode useColor history' = do
  minput <- getUserInput useColor
  case minput of
    Nothing -> outputStrLn "Goodbye!"
    Just userInput
      | isExitCommand userInput -> outputStrLn "Goodbye!"
      | otherwise -> do
          result <- liftIO $ executeRequest opts apiKey' history' mcpCtx userInput
          handleResponse opts apiKey' mcpClients mcpCtx colorMode useColor history' userInput result

getUserInput :: Bool -> InputT IO (Maybe T.Text)
getUserInput useColor = do
  let greenColor = setSGRCode [SetColor Foreground Vivid Green]
      resetColor = setSGRCode [Reset]
      coloredPrompt = if useColor
                      then greenColor ++ "> " ++ resetColor ++ greenColor
                      else "> "
  minput <- getInputLine coloredPrompt
  when useColor $ liftIO $ putStr resetColor
  return $ T.pack <$> minput

isExitCommand :: T.Text -> Bool
isExitCommand input = input `elem` ["exit", "quit", ":q"]

executeRequest :: Options -> Maybe T.Text -> ConversationHistory -> Maybe MCPContext -> T.Text -> IO (Either LLMError LLMResponse)
executeRequest opts apiKey' history' mcpCtx userInput = do
  let request = LLMRequest
        { prompt = userInput
        , model = modelName opts
        , apiKey = apiKey'
        , baseUrl = baseUrlOpt opts
        , streaming = Data.Maybe.fromMaybe False (streamOpt opts)
        , history = history'
        , mcpContext = mcpCtx
        }
  startSpinner
  result <- callProvider opts request
  stopSpinner
  return result

handleResponse :: Options -> Maybe T.Text -> [MCPClient] -> Maybe MCPContext -> ColorMode -> Bool -> ConversationHistory -> T.Text -> Either LLMError LLMResponse -> InputT IO ()
handleResponse opts apiKey' mcpClients mcpCtx colorMode useColor history' _userInput (Left err) = do
  errorText <- liftIO $ Color.errorColor colorMode $ "Error: " <> T.pack (show err)
  liftIO $ TIO.hPutStrLn stderr errorText
  conversationLoop opts apiKey' mcpClients mcpCtx colorMode useColor history'
handleResponse opts apiKey' mcpClients mcpCtx colorMode useColor history' userInput' (Right response) =
  case Types.toolCalls response of
    Just calls | not (null calls) ->
      handleToolCallResponse opts apiKey' mcpClients mcpCtx colorMode useColor history' userInput' response calls
    _ ->
      handleNormalResponse opts apiKey' mcpClients mcpCtx colorMode useColor history' userInput' response

handleToolCallResponse :: Options -> Maybe T.Text -> [MCPClient] -> Maybe MCPContext -> ColorMode -> Bool -> ConversationHistory -> T.Text -> LLMResponse -> [Types.ToolCall] -> InputT IO ()
handleToolCallResponse opts apiKey' mcpClients mcpCtx colorMode useColor history' userInput response calls = do
  handleToolCallResponseWithDepth opts apiKey' mcpClients mcpCtx colorMode useColor history' userInput response calls 0

handleToolCallResponseWithDepth :: Options -> Maybe T.Text -> [MCPClient] -> Maybe MCPContext -> ColorMode -> Bool -> ConversationHistory -> T.Text -> LLMResponse -> [Types.ToolCall] -> Int -> InputT IO ()
handleToolCallResponseWithDepth opts apiKey' mcpClients mcpCtx colorMode useColor history' userInput response calls depth = do
  when (depth >= 5) $ do
    errorText <- liftIO $ Color.errorColor colorMode "Error: Too many tool call iterations (max 5). Stopping."
    liftIO $ TIO.hPutStrLn stderr errorText
    conversationLoop opts apiKey' mcpClients mcpCtx colorMode useColor history'

  let assistantMsg = content response
  unless (T.null assistantMsg) $
    liftIO $ TIO.putStrLn assistantMsg

  liftIO $ TIO.putStrLn ""
  toolResults <- liftIO $ mapM (executeToolCall mcpClients colorMode) calls
  liftIO $ TIO.putStrLn ""

  let toolResultText = buildToolResultText calls toolResults
      updatedHistory = history' ++ [Message "user" userInput]

  result' <- liftIO $ executeToolResultRequest opts apiKey' updatedHistory toolResultText
  handleToolResultResponseWithDepth opts apiKey' mcpClients mcpCtx colorMode useColor updatedHistory depth result'

handleToolResultResponseWithDepth :: Options -> Maybe T.Text -> [MCPClient] -> Maybe MCPContext -> ColorMode -> Bool -> ConversationHistory -> Int -> Either LLMError LLMResponse -> InputT IO ()
handleToolResultResponseWithDepth opts apiKey' mcpClients mcpCtx colorMode useColor updatedHistory _ (Left err) = do
  errorText <- liftIO $ Color.errorColor colorMode $ "Error: " <> T.pack (show err)
  liftIO $ TIO.hPutStrLn stderr errorText
  conversationLoop opts apiKey' mcpClients mcpCtx colorMode useColor updatedHistory
handleToolResultResponseWithDepth opts apiKey' mcpClients mcpCtx colorMode useColor updatedHistory depth (Right response') = do
  let assistantContent = content response'
  -- Check if response has tool calls again
  case Types.toolCalls response' of
    Just calls | not (null calls) ->
      -- Execute these tool calls too (recursive call)
      handleToolCallResponseWithDepth opts apiKey' mcpClients mcpCtx colorMode useColor updatedHistory "" response' calls (depth + 1)
    _ -> do
      -- Only print newline if streaming was disabled (content is not empty)
      unless (Data.Maybe.fromMaybe False (streamOpt opts)) $
        liftIO $ TIO.putStrLn ""
      liftIO $ TIO.putStrLn ""
      let finalHistory = buildFinalHistory updatedHistory assistantContent
      conversationLoop opts apiKey' mcpClients mcpCtx colorMode useColor finalHistory

buildToolResultText :: [Types.ToolCall] -> [T.Text] -> T.Text
buildToolResultText calls results =
  T.intercalate "\n\n"
    [ "Tool: " <> Types.toolName tc <> "\nResult: " <> res
    | (tc, res) <- zip calls results
    ]

executeToolResultRequest :: Options -> Maybe T.Text -> ConversationHistory -> T.Text -> IO (Either LLMError LLMResponse)
executeToolResultRequest opts apiKey' history' toolResultText = do
  let request = LLMRequest
        { prompt = "Based on the tool results below, please provide a final answer to the user's question. Do not call any more tools.\n\n" <> toolResultText
        , model = modelName opts
        , apiKey = apiKey'
        , baseUrl = baseUrlOpt opts
        , streaming = Data.Maybe.fromMaybe False (streamOpt opts)  -- Use user's streaming preference
        , history = history'
        , mcpContext = Nothing
        }
  startSpinner
  result <- callProvider opts request
  stopSpinner
  return result

buildFinalHistory :: ConversationHistory -> T.Text -> ConversationHistory
buildFinalHistory hist assistantContent =
  if T.null assistantContent
    then hist ++ [Message "assistant" "[Response from tool results]"]
    else hist ++ [Message "assistant" assistantContent]

handleNormalResponse :: Options -> Maybe T.Text -> [MCPClient] -> Maybe MCPContext -> ColorMode -> Bool -> ConversationHistory -> T.Text -> LLMResponse -> InputT IO ()
handleNormalResponse opts apiKey' mcpClients mcpCtx colorMode useColor history' userInput response = do
  let assistantMsg = content response

  if Data.Maybe.fromMaybe False (streamOpt opts)
    then liftIO $ TIO.putStrLn ""
    else liftIO $ TIO.putStrLn assistantMsg

  liftIO $ hFlush stdout
  liftIO $ TIO.putStrLn ""

  let assistantContent = if T.null assistantMsg
                        then "[Streamed response]"
                        else assistantMsg
      updatedHistory = history' ++ [Message "user" userInput, Message "assistant" assistantContent]

  conversationLoop opts apiKey' mcpClients mcpCtx colorMode useColor updatedHistory
