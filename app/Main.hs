module Main where

import Commands.Eval (processEval)
import Commands.Info
import Commands.Numbered
import Data.Text (Text)
import Fmt ((+|), (|+))
import Options.Applicative hiding (action)
import Parser
import RIO (readFileUtf8)
import System.Directory (doesFileExist)
import qualified TuringMachines.Parser as TP
import Utils

main :: IO ()
main = doWork =<< execParser opts
  where
    opts =
      info
        (options <**> helper)
        ( fullDesc
            <> header "turing-machines - Un interprete de maquinas de Turing"
        )

doWork :: Commands -> IO ()
doWork (Numbered n) = processNumbered n
doWork (Info path opts) = withExistentFile path (processInfo opts)
doWork (Eval path input opts) = withExistentFile path $ \pathText -> do
  case TP.parse pathText of
    Just program -> processEval opts program input
    Nothing -> exitError "El programa no es valido"

withExistentFile :: FilePath -> (Text -> IO ()) -> IO ()
withExistentFile path action = do
  exists <- doesFileExist path
  if exists
    then readFileUtf8 path >>= action
    else exitError $ "El archivo '" +| path |+ "' no existe"
