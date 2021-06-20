module ParserUtils where

import Data.Foldable (find)
import Options.Applicative
import Text.Read (readMaybe)

positiveIntParser :: ReadM Int
positiveIntParser =
  eitherReader
    ( maybe
        (Left "Debe ser un entero mayor o igual a cero")
        Right
        . find (>= 0)
        . readMaybe
    )

fileArgument :: Parser FilePath
fileArgument = argument str (metavar "FILE" <> help "El archivo que contiene el programa")
