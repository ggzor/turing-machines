module Utils where

import Data.Text (unpack)
import Fmt (Builder, fmtLn)
import System.Exit (exitFailure)
import TuringMachines.Core
import qualified TuringMachines.PPrint as PP
import Utils.QString

programAsStr :: Program Integer -> String
programAsStr = unpack . PP.pprint . mapProgram QString

exitError :: Builder -> IO ()
exitError message = do
  fmtLn message
  exitFailure
