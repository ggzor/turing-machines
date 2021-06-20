{-# LANGUAGE ApplicativeDo #-}

module Parser where

import Data.Foldable (find, fold)
import qualified Data.List as L
import Options.Applicative hiding (action)
import RIO.Prelude (readMaybe)
import RIO.Text (pack)
import SVG (RenderOptions (RenderOptions, _cellGap, _cellSize, _minWidth, _tapeHeight))
import Text.Megaparsec (parseMaybe)
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
  { doNotEvalSpeculatively :: !Bool
  , lineByLine :: !Bool
  , limitSteps :: Maybe Int
  , stepOutput :: Maybe FilePath
  , outputDirectory :: Maybe FilePath
  , renderOptions :: !RenderOptions
  }

options :: Parser Commands
options =
  subparser . fold $
    [ command "numbered" . info (numberedOptions <**> helper) $
        progDesc "Muestra el programa correspondiente al numero dado"
    , command "info" . info (infoOptions <**> helper) $
        progDesc "Muestra información relevante del programa dado"
    , command "eval" . info (evalOptions <**> helper) $
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

evalOptions :: Parser Commands
evalOptions =
  Eval <$> fileArgument
    <*> argument
      (maybeReader (parseMaybe TP.pTape . pack))
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
                  positiveIntParser
                  ( short 's'
                      <> long "steps"
                      <> metavar "N"
                      <> help "La cantidad de pasos por ejecutar"
                  )
              )
            <*> optional
              ( option
                  str
                  ( short 'o'
                      <> long "out-image"
                      <> metavar "OUT_IMAGE"
                      <> help "La imagen a generar por cada iteracion"
                  )
              )
            <*> optional
              ( option
                  str
                  ( short 'd'
                      <> long "directory"
                      <> metavar "OUT_DIR"
                      <> help
                        ( "El directorio en el cual generar los pasos de evaluación"
                            ++ ". Implica no evaluar en tiempo real, a menos que se especifique --image"
                        )
                  )
              )
            <*> renderOptionsOptions
        )

renderOptionsOptions :: Parser RenderOptions
renderOptionsOptions = do
  _minWidth <-
    option
      positiveIntParser
      ( short 'w'
          <> metavar "INT"
          <> value 400
          <> showDefault
          <> help "El ancho minimo de la salida"
      )

  _cellSize <-
    option
      positiveIntParser
      ( short 'c'
          <> metavar "INT"
          <> value 30
          <> showDefault
          <> help "La medida de cada celda de la cinta"
      )

  _tapeHeight <-
    option
      positiveIntParser
      ( long "tape-height"
          <> metavar "INT"
          <> value 120
          <> showDefault
          <> help "El alto de la cinta"
      )

  _cellGap <-
    option
      positiveIntParser
      ( long "cell-gap"
          <> metavar "INT"
          <> value 10
          <> showDefault
          <> help "La separación entre las celdas"
      )

  pure
    RenderOptions
      { _tapeHeight
      , _cellSize
      , _cellGap
      , _minWidth
      }

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
