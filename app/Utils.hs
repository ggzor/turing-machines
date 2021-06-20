module Utils where

import TuringMachines.Core
import qualified TuringMachines.PPrint as PP

import Data.Text (Text, unpack)
import qualified Data.Text.IO as TIO
import System.Exit (exitFailure)
import Utils.QString

programAsStr :: Program Integer -> String
programAsStr = unpack . PP.pprint . mapProgram QString

exitError :: Text -> IO ()
exitError message = do
  TIO.putStrLn message
  exitFailure
