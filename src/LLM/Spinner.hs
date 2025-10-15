{-# LANGUAGE OverloadedStrings #-}

module LLM.Spinner
  ( globalSpinnerThread
  , startSpinner
  , stopSpinner
  ) where

import Control.Concurrent (ThreadId, killThread, forkIO, threadDelay)
import Data.IORef (IORef, newIORef, atomicModifyIORef', writeIORef)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.IO (hFlush, stdout)
import System.IO.Unsafe (unsafePerformIO)

-- Global spinner thread ID for stopping spinner from stream processors
{-# NOINLINE globalSpinnerThread #-}
globalSpinnerThread :: IORef (Maybe ThreadId)
globalSpinnerThread = unsafePerformIO $ newIORef Nothing

-- Start the spinner
startSpinner :: IO ()
startSpinner = do
  let spinChars = ['⠋', '⠙', '⠹', '⠸', '⠼', '⠴', '⠦', '⠧', '⠇', '⠏']
      spinner = do
        mapM_ (\c -> do
          TIO.putStr $ "\r" <> T.singleton c <> " Thinking..."
          hFlush stdout
          threadDelay 80000  -- 80ms
          ) spinChars
        spinner  -- Loop forever
  tid <- forkIO spinner
  writeIORef globalSpinnerThread (Just tid)

-- Stop the spinner (called from stream processors or after action completes)
stopSpinner :: IO ()
stopSpinner = do
  mTid <- atomicModifyIORef' globalSpinnerThread (\tid -> (Nothing, tid))
  case mTid of
    Just tid -> do
      killThread tid
      TIO.putStr "\r\x1b[K"  -- Clear the spinner line
      hFlush stdout
    Nothing -> return ()
