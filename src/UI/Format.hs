{-# LANGUAGE OverloadedStrings #-}

module UI.Format
  ( drawBox
  , textWidth
  , BoxLine(..)
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Char.WCWidth (wcwidth)

-- | Represents different parts of a box line
data BoxLine = BoxLine
  { boxPrefix :: Text   -- Left border (e.g., "│ ")
  , boxContent :: Text  -- The content part
  , boxSuffix :: Text   -- Right border (e.g., " │")
  } deriving (Show, Eq)

-- | Calculate display width of text (considering full-width characters)
textWidth :: Text -> Int
textWidth = sum . map (max 0 . wcwidth) . T.unpack

-- | Draw a box around text content (returns structured data for coloring)
drawBox :: Text -> [Text] -> [BoxLine]
drawBox title content =
  let allLines = title : content
      maxLen = maximum $ map textWidth allLines
      border c = c <> "─" <> T.replicate maxLen "─" <> "─" <> mirrorChar c

      padLine line =
        let width = textWidth line
            padding = maxLen - width
        in BoxLine "│ " (line <> T.replicate padding " ") " │"

  in BoxLine (border "┌") "" ""
   : padLine title
   : BoxLine (border "├") "" ""
   : map padLine content
  ++ [BoxLine (border "└") "" ""]
  where
    mirrorChar "┌" = "┐"
    mirrorChar "├" = "┤"
    mirrorChar "└" = "┘"
    mirrorChar _ = ""
