module Main where

import Commands.Eval.Parser (EvalCommandOptions (..))
import Commands.Eval.Runner (processEval)
import Commands.Info.Parser (InfoCommandOptions (..))
import Commands.Info.Runner (processInfo)
import Commands.Numbered.Parser (NumberedCommandOptions (..))
import Commands.Numbered.Runner (processNumbered)

import TuringMachines.Core
import qualified TuringMachines.Parser as TP

import Parser
import Utils

import Data.String.Interpolate
import Data.Text (Text)
import Options.Applicative hiding (action)
import RIO (readFileUtf8)
import System.Console.ANSI
import System.Directory (doesFileExist)
import Text.Megaparsec (errorBundlePretty, parse)

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
doWork (Numbered (NumberedCommandOptions n)) = processNumbered n
doWork (Info (InfoCommandOptions path opts)) =
  withExistentFile path \pathText ->
    withValidProgram path pathText (processInfo opts)
doWork (Eval (EvalCommandOptions path input opts)) =
  withExistentFile path \pathText ->
    withValidProgram path pathText \program ->
      processEval opts program input

withExistentFile :: FilePath -> (Text -> IO ()) -> IO ()
withExistentFile path action = do
  exists <- doesFileExist path
  if exists
    then readFileUtf8 path >>= action
    else exitError [i|"El archivo #{path} no existe"|]

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
