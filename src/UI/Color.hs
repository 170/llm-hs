{-# LANGUAGE OverloadedStrings #-}

module UI.Color
  ( ColorMode(..)
  , withColor
  , systemColor
  , userColor
  , assistantColor
  , errorColor
  , infoColor
  , toolColor
  , shouldUseColor
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Core.Types (ColorMode(..))
import System.Console.ANSI
import System.IO (hIsTerminalDevice, stdout)

-- | Determine if color should be used based on ColorMode
shouldUseColor :: ColorMode -> IO Bool
shouldUseColor NoColor = return False
shouldUseColor AlwaysColor = return True
shouldUseColor AutoColor = hIsTerminalDevice stdout

-- | Apply color to text if color mode allows
withColor :: ColorMode -> [SGR] -> Text -> IO Text
withColor mode sgrs text = do
  useColor <- shouldUseColor mode
  if useColor
    then return $ T.pack (setSGRCode sgrs) <> text <> T.pack (setSGRCode [Reset])
    else return text

-- | Color for system messages (cyan)
systemColor :: ColorMode -> Text -> IO Text
systemColor mode = withColor mode [SetColor Foreground Vivid Cyan]

-- | Color for user messages (green)
userColor :: ColorMode -> Text -> IO Text
userColor mode = withColor mode [SetColor Foreground Vivid Green]

-- | Color for assistant messages (blue)
assistantColor :: ColorMode -> Text -> IO Text
assistantColor mode = withColor mode [SetColor Foreground Vivid Blue]

-- | Color for error messages (red)
errorColor :: ColorMode -> Text -> IO Text
errorColor mode = withColor mode [SetColor Foreground Vivid Red]

-- | Color for info messages (yellow)
infoColor :: ColorMode -> Text -> IO Text
infoColor mode = withColor mode [SetColor Foreground Vivid Yellow]

-- | Color for tool messages (magenta)
toolColor :: ColorMode -> Text -> IO Text
toolColor mode = withColor mode [SetColor Foreground Vivid Magenta]
