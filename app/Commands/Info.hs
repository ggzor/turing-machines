module Commands.Info where

import Control.Monad (forM_, void)
import Data.Text (Text)
import Fmt (fmt, fmtLn, (+|), (|+))
import Math.Primes (primes)
import Parser
import Text.Megaparsec (parseMaybe)
import TuringMachines.Graphviz
import TuringMachines.Normalize (getProgram, normalize)
import TuringMachines.Numbering
import qualified TuringMachines.Parser as TP
import Utils

processInfo :: InfoOptions -> Text -> IO ()
processInfo (InfoOptions format) t =
  case parseMaybe TP.pProgram t of
    Nothing -> putStrLn "El programa no es valido"
    Just p -> do
      let label = if length format > 1 then putStrLn else void . pure
      let normalized = normalize p

      if format == [Normalized, Graph]
        then fmtLn $ generateGraph (getProgram normalized) |+ ""
        else do
          forM_ format $ \case
            Original -> do
              label "Original: "
              putStrLn $ programAsStr p
            Normalized -> do
              label "Normalizado: "
              putStrLn $ programAsStr (getProgram normalized)
            Number -> do
              label "Numero de Godel:"
              print $ programAsNumber normalized
            PrimeSeq -> do
              label "Secuencia de potencias de primos: "
              putStrLn $
                unwords . zipWith (\prime n -> fmt $ prime |+ "^" +| n |+ "") primes $
                  programAsSequence normalized
            Graph -> do
              label "Grafo: "
              fmtLn $ generateGraph p |+ ""
