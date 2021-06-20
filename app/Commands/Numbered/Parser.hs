module Commands.Numbered.Parser where

import Options.Applicative

data NumberedCommandOptions = NumberedCommandOptions !Integer

numberedCommandOptions :: Parser NumberedCommandOptions
numberedCommandOptions =
  NumberedCommandOptions
    <$> argument auto (metavar "N" <> help "El numero del programa a mostrar")
