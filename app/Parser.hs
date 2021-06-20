{-# LANGUAGE ApplicativeDo #-}

module Parser where

import Commands.Eval.Parser (EvalCommandOptions, evalCommandOptions)
import Data.Foldable (fold)
import qualified Data.List as L
import Options.Applicative hiding (action)
import ParserUtils

data Commands
  = Numbered !Integer
  | Info !FilePath !InfoOptions
  | Eval !EvalCommandOptions

data FormatOption = Original | Normalized | Number | PrimeSeq | Graph
  deriving (Ord, Eq, Show)

newtype InfoOptions = InfoOptions [FormatOption]

options :: Parser Commands
options =
  subparser . fold $
    [ command "numbered" . info (numberedOptions <**> helper) $
        progDesc "Muestra el programa correspondiente al numero dado"
    , command "info" . info (infoOptions <**> helper) $
        progDesc "Muestra información relevante del programa dado"
    , command "eval" . info (Eval <$> evalCommandOptions <**> helper) $
        progDesc "Evalua el programa con la entrada dada"
    ]

numberedOptions :: Parser Commands
numberedOptions =
  Numbered
    <$> argument auto (metavar "N" <> help "El numero del programa a mostrar")

infoOptions :: Parser Commands
infoOptions =
  Info
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
