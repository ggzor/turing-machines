{-# LANGUAGE ApplicativeDo #-}

module Commands.Eval.Parser where

import Data.Text (pack)
import Options.Applicative
import ParserUtils
import SVG (RenderOptions (RenderOptions), _cellGap, _cellSize, _minWidth, _tapeHeight)
import Text.Megaparsec (parseMaybe)
import TuringMachines.Core
import qualified TuringMachines.Parser as TP

data EvalCommandOptions = EvalCommandOptions !FilePath !Tape !EvalOptions

data EvalOptions = EvalOptions
  { doNotEvalSpeculatively :: !Bool
  , lineByLine :: !Bool
  , limitSteps :: Maybe Int
  , stepOutput :: Maybe FilePath
  , outputDirectory :: Maybe FilePath
  , renderOptions :: !RenderOptions
  }

evalCommandOptions :: Parser EvalCommandOptions
evalCommandOptions =
  EvalCommandOptions <$> fileArgument
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
