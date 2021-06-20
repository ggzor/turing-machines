module Commands.Info.Parser where

import Data.Foldable (fold)
import qualified Data.List as L
import Options.Applicative
import ParserUtils

data InfoCommandOptions = InfoCommandOptions !FilePath !InfoOptions

newtype InfoOptions = InfoOptions [FormatOption]

data FormatOption = Original | Normalized | Number | PrimeSeq | Graph
  deriving (Ord, Eq, Show)

infoOptions :: Parser InfoCommandOptions
infoOptions =
  InfoCommandOptions
    <$> fileArgument
    <*> infoOptionsOptions

infoOptionsOptions :: Parser InfoOptions
infoOptionsOptions =
  InfoOptions
    <$> option
      (eitherReader parseFormatOptionList)
      ( short 'f'
          <> long "format"
          <> metavar "FORMAT"
          <> value [Original, Normalized, Number, PrimeSeq]
          <> help
            ( fold
                [ "Formatos disponibles:"
                , " o = Programa original"
                , ", n = Programa normalizado"
                , ", u = Numero de Godel"
                , ", s = Sequencia de primos"
                , ", g = Grafo"
                ]
            )
      )

parseFormatOptionList :: String -> Either String [FormatOption]
parseFormatOptionList = fmap L.nub . traverse parseFormatOption

parseFormatOption :: Char -> Either String FormatOption
parseFormatOption 'o' = Right Original
parseFormatOption 'n' = Right Normalized
parseFormatOption 'u' = Right Number
parseFormatOption 's' = Right PrimeSeq
parseFormatOption 'g' = Right Graph
parseFormatOption c = Left $ "Formato desconocido: " ++ [c]
