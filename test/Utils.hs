module Utils where

import qualified Data.Text as T

skipFirstLine :: T.Text -> T.Text
skipFirstLine = T.unlines . tail . T.lines

skipLastLine :: T.Text -> T.Text
skipLastLine = T.unlines . init . T.lines

skipFirstLastLine :: T.Text -> T.Text
skipFirstLastLine = T.unlines . tail . init . T.lines
