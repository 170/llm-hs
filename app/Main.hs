module Main (main) where

import Lib
import LLM.CLI (parseOptions)

main :: IO ()
main = do
  opts <- parseOptions
  runLLM opts
