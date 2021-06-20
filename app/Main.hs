module Main where

import Commands.Eval (processEval)
import Commands.Info
import Commands.Numbered
import Data.Text (Text)
import Fmt ((+|), (|+))
import Options.Applicative hiding (action)
import Parser
import RIO (readFileUtf8)
import System.Console.ANSI
import System.Directory (doesFileExist)
import Text.Megaparsec (errorBundlePretty, parse)
import TuringMachines.Core
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
doWork (Info path opts) =
  withExistentFile path \pathText ->
    withValidProgram path pathText (processInfo opts)
doWork (Eval path input opts) =
  withExistentFile path \pathText ->
    withValidProgram path pathText \program ->
      processEval opts program input

withExistentFile :: FilePath -> (Text -> IO ()) -> IO ()
withExistentFile path action = do
  exists <- doesFileExist path
  if exists
    then readFileUtf8 path >>= action
    else exitError $ "El archivo '" +| path |+ "' no existe"

withValidProgram :: FilePath -> Text -> (Program Integer -> IO ()) -> IO ()
withValidProgram path programText action = do
  case parse TP.pProgram path programText of
    Right program -> action program
    Left err -> do
      setSGR [SetColor Foreground Vivid Red, SetConsoleIntensity BoldIntensity]
      putStr "error: "
      setSGR [Reset]
      putStrLn "El programa no es válido"
      putStr (errorBundlePretty err)
