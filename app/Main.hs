module Main where

import Commands.Abacus.Parser (AbacusCommandOptions (..))
import Commands.Abacus.Runner (processAbacus)
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
import System.Directory (doesFileExist)
import System.Environment
import Text.Megaparsec (errorBundlePretty, parse)

main :: IO ()
main = do
  setEnv "LC_ALL" "C.UTF-8"
  doWork =<< execParser opts
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
doWork (Abacus (AbacusCommandOptions path opts)) =
  withExistentFile path (`processAbacus` opts)

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
      printError "El programa no es valido"
      putStr (errorBundlePretty err)
