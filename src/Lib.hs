{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( runLLM
  ) where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (lookupEnv)
import System.IO (stderr, hFlush, stdout, stdin, hIsTerminalDevice)
import System.Console.Haskeline
import Control.Monad.IO.Class (liftIO)
import LLM.Types
import LLM.OpenAI (callOpenAI)
import LLM.Claude (callClaude)
import LLM.Ollama (callOllama)
import LLM.Gemini (callGemini)
import LLM.CLI (Options(..))
import LLM.Spinner (startSpinner, stopSpinner)

runLLM :: Options -> IO ()
runLLM opts = do
  -- Check if stdin is a terminal (interactive mode)
  isTerminal <- hIsTerminalDevice stdin

  if isTerminal
    then runInteractive opts
    else runPipe opts

runPipe :: Options -> IO ()
runPipe opts = do
  -- Read input from stdin
  input <- TIO.getContents

  -- Get API key
  apiKeyValue <- getApiKey opts

  let request = LLMRequest
        { prompt = input
        , model = modelName opts
        , apiKey = apiKeyValue
        , baseUrl = baseUrlOpt opts
        , streaming = maybe False id (streamOpt opts)
        , history = []  -- No history in pipe mode
        }

  -- Call the appropriate provider
  result <- callProvider opts request

  -- Output the result
  handleResult opts result

runInteractive :: Options -> IO ()
runInteractive opts = do
  let prov = case provider opts of
        Just p -> p
        Nothing -> error "Provider must be set"  -- Should never happen after merge
  TIO.putStrLn "=== Interactive Mode ==="
  TIO.putStrLn $ "Provider: " <> T.pack (show prov)
  let defaultModel = case prov of
        OpenAI -> "gpt-4o-mini"
        Claude -> "claude-3-5-sonnet-20241022"
        Ollama -> "llama3.2"
        Gemini -> "gemini-1.5-flash"
  case modelName opts of
    Just model -> TIO.putStrLn $ "Model: " <> model
    Nothing -> TIO.putStrLn $ "Model: " <> defaultModel <> " (default)"
  TIO.putStrLn "Type your message and press Enter. Type 'exit' or 'quit' to end."
  TIO.putStrLn "Emacs keybindings supported (C-a, C-e, C-k, etc.)\n"

  -- Get API key
  apiKeyValue <- getApiKey opts

  -- Start conversation loop with haskeline
  runInputT defaultSettings $ conversationLoop opts apiKeyValue []

conversationLoop :: Options -> Maybe T.Text -> ConversationHistory -> InputT IO ()
conversationLoop opts apiKey history = do
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
                }

          -- Show spinner in interactive mode (always)
          liftIO startSpinner
          result <- liftIO $ callProvider opts request
          liftIO stopSpinner

          -- Handle the result and update history
          case result of
            Left err -> do
              liftIO $ TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
              conversationLoop opts apiKey history
            Right response -> do
              let assistantMsg = content response

              if maybe False id (streamOpt opts)
                then liftIO $ TIO.putStrLn ""  -- Add newline after streaming
                else liftIO $ TIO.putStrLn assistantMsg

              liftIO $ hFlush stdout
              liftIO $ TIO.putStrLn ""

              -- Add both user and assistant messages to history
              let updatedHistory = history ++ [Message "user" userInput, Message "assistant" assistantMsg]

              -- Continue conversation
              conversationLoop opts apiKey updatedHistory


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

callProvider :: Options -> LLMRequest -> IO (Either LLMError LLMResponse)
callProvider opts request =
  let prov = case provider opts of
        Just p -> p
        Nothing -> error "Provider must be set"
  in case prov of
    OpenAI -> callOpenAI request
    Claude -> callClaude request
    Ollama -> callOllama request
    Gemini -> callGemini request

handleResult :: Options -> Either LLMError LLMResponse -> IO ()
handleResult opts result = case result of
  Left err -> TIO.hPutStrLn stderr $ "Error: " <> T.pack (show err)
  Right response -> do
    if maybe False id (streamOpt opts)
      then TIO.putStrLn ""  -- Add newline after streaming
      else TIO.putStrLn $ content response
    hFlush stdout
