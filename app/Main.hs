module Main (main) where

import Lib
import LLM.CLI (parseOptions, mergeConfigWithOptions)
import LLM.Config (loadConfig)
import System.Exit (die)

main :: IO ()
main = do
  -- Load configuration file if it exists
  config <- loadConfig

  -- Parse command-line options
  cliOpts <- parseOptions

  -- Merge config file with CLI options (CLI takes precedence)
  case mergeConfigWithOptions config cliOpts of
    Left err -> die err
    Right opts -> runLLM opts
