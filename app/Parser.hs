module Parser where

import Data.Foldable (find, fold)
import qualified Data.List as L
import Options.Applicative hiding (action)
import RIO.Prelude (readMaybe)
import RIO.Text (pack)
import TuringMachines.Core
import qualified TuringMachines.Parser as TP

data Commands
  = Numbered !Integer
  | Info !FilePath !InfoOptions
  | Eval !FilePath !Tape !EvalOptions

data FormatOption = Original | Normalized | Number | PrimeSeq | Graph
  deriving (Ord, Eq, Show)

newtype InfoOptions = InfoOptions [FormatOption]

data EvalOptions = EvalOptions
  { doNotEvalSpeculatively :: !Bool,
    lineByLine :: !Bool,
    limitSteps :: Maybe Integer
  }

options :: Parser Commands
options =
  subparser . fold $
    [ command "numbered" . info numberedOptions $
        progDesc "Muestra el programa correspondiente al numero dado",
      command "info" . info (infoOptions <**> helper) $
        progDesc "Muestra información relevante del programa dado",
      command "eval" . info (evalOptions <**> helper) $
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
                [ "Formatos disponibles:",
                  " o = Programa original",
                  ", n = Programa normalizado",
                  ", u = Numero de Godel",
                  ", s = Sequencia de primos",
                  ", g = Grafo"
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

evalOptions :: Parser Commands
evalOptions =
  Eval <$> fileArgument
    <*> argument
      (maybeReader (TP.parseTape . pack))
      (metavar "INPUT" <> help "La entrada binaria para el programa")
    <*> ( EvalOptions
            <$> switch
              ( short 'n'
                  <> help
                    ( "No evaluar especulativamente: "
                        ++ "Provoca una salida menos agradable pero al menos muestra salida"
                    )
              )
            <*> switch
              ( short 'l'
                  <> help "Evaluar linea por linea"
              )
            <*> optional
              ( option
                  ( eitherReader
                      ( maybe
                          (Left "Debe ser un entero mayor o igual a cero")
                          Right
                          . find (>= 0)
                          . readMaybe
                      )
                  )
                  ( short 's'
                      <> long "steps"
                      <> metavar "N"
                      <> help "La cantidad de pasos por ejecutar"
                  )
              )
        )

fileArgument :: Parser FilePath
fileArgument = argument str (metavar "FILE" <> help "El archivo que contiene el programa")
