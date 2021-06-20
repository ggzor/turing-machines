module Commands.Info.Runner where

import Commands.Info.Parser

import TuringMachines.Core
import TuringMachines.Graphviz
import TuringMachines.Normalize (getProgram, normalize)
import TuringMachines.Numbering

import Math.Primes (primes)
import Utils

import Control.Monad (forM_, void)
import Data.String.Interpolate
import qualified Data.Text.IO as TIO

processInfo :: InfoOptions -> Program Integer -> IO ()
processInfo (InfoOptions format) p = do
  let label = if length format > 1 then putStrLn else void . pure
  let normalized = normalize p

  if format == [Normalized, Graph]
    then TIO.putStrLn $ generateGraph (getProgram normalized)
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
            unwords . zipWith (\prime n -> [i|#{prime}^#{n}|]) primes $
              programAsSequence normalized
        Graph -> do
          label "Grafo: "
          TIO.putStrLn $ generateGraph p
