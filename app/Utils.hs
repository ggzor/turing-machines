module Utils where

import TuringMachines.Core
import qualified TuringMachines.PPrint as PP

import Data.Text (Text, unpack)
import qualified Data.Text.IO as TIO
import System.Console.ANSI
import System.Exit (exitFailure)
import Utils.QString

programAsStr :: Program Integer -> String
programAsStr = unpack . PP.pprint . mapProgram QString

exitError :: Text -> IO ()
exitError message = do
  printError message
  exitFailure

printError :: Text -> IO ()
printError err = do
  setSGR [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
  TIO.putStr "error: "
  setSGR [Reset]
  TIO.putStrLn err
