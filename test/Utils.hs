module Utils where

import qualified Data.Text as T

skipFirstLine :: T.Text -> T.Text
skipFirstLine = T.intercalate "\n" . tail . T.lines

skipLastLine :: T.Text -> T.Text
skipLastLine = T.intercalate "\n" . init . T.lines

skipFirstLastLine :: T.Text -> T.Text
skipFirstLastLine = T.intercalate "\n" . tail . init . T.lines
