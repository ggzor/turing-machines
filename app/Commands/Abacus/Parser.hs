module Commands.Abacus.Parser where

import Data.Foldable (fold)
import Options.Applicative
import ParserUtils

data AbacusCommandOptions = AbacusCommandOptions !FilePath !AbacusSubCommandOptions

newtype AbacusSubCommandOptions = Compile String

data FormatOption = Original | Normalized | Number | PrimeSeq | Graph
  deriving (Ord, Eq, Show)

abacusOptions :: Parser AbacusCommandOptions
abacusOptions =
  AbacusCommandOptions
    <$> fileArgument
    <*> abacusSubCommandOptions

abacusSubCommandOptions :: Parser AbacusSubCommandOptions
abacusSubCommandOptions =
  subparser . fold $
    [ command "compile" . info (compileOptions <**> helper) $
        progDesc "Compila la macro con el nombre dado"
    ]

compileOptions :: Parser AbacusSubCommandOptions
compileOptions =
  Compile
    <$> argument
      str
      (metavar "MACRO_NAME" <> help "El nombre de la macro a compilar")
