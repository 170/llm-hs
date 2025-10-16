{-# LANGUAGE OverloadedStrings #-}

module LLM.Format
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
textWidth = sum . map charWidth . T.unpack
  where
    charWidth c = max 0 (wcwidth c)  -- wcwidth returns -1 for control characters

-- | Draw a box around text content (returns structured data for coloring)
drawBox :: Text -> [Text] -> [BoxLine]
drawBox title content =
  let maxLen = maximum $ map textWidth (title : content)
      topBorder = "┌─" <> T.replicate maxLen "─" <> "─┐"
      bottomBorder = "└─" <> T.replicate maxLen "─" <> "─┘"
      separator = "├─" <> T.replicate maxLen "─" <> "─┤"

      padLine line =
        let width = textWidth line
            padding = maxLen - width
        in BoxLine "│ " (line <> T.replicate padding " ") " │"

      titleLine = padLine title
      contentLines = map padLine content

  in [ BoxLine topBorder "" ""
     , titleLine
     , BoxLine separator "" ""
     ] ++ contentLines ++ [BoxLine bottomBorder "" ""]
