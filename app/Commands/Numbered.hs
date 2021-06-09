module Commands.Numbered where

import TuringMachines.Numbering
import Utils

processNumbered :: Integer -> IO ()
processNumbered n =
  case numberAsProgram n of
    Nothing -> do
      exitError "El numero no representa un programa valido"
    Just p -> do
      putStrLn (programAsStr p)
