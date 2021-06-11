module Main where

import Commands.Eval (processEval)
import Commands.Info
import Commands.Numbered
import Data.Function ((&))
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Fmt ((+|), (|+))
import Options.Applicative hiding (action)
import Parser
import RIO (readFileUtf8)
import System.Directory (doesFileExist)
import TuringMachines.Core
import TuringMachines.Eval (eval)
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
doWork (Eval path input opts@EvalOptions{doNotEvalSpeculatively}) = withExistentFile path $ \pathText -> do
  case TP.parse pathText of
    Just program ->
      let initialIdx = 0
          initialQ = 1
          (minIdx, maxIdx) = dangerouslyDetermineBounds program (State initialQ initialIdx input)
          adjustedTape =
            input
              & IntMap.insert minIdx (fromMaybe B0 (IntMap.lookup minIdx input))
              & IntMap.insert maxIdx (fromMaybe B0 (IntMap.lookup maxIdx input))
          initialState =
            State initialQ initialIdx $
              if doNotEvalSpeculatively
                then input
                else adjustedTape
       in processEval opts program initialState
    Nothing -> exitError "El programa no es valido"

dangerouslyDetermineBounds :: forall a. (Ord a) => Program a -> State a -> (Index, Index)
dangerouslyDetermineBounds prog st@(State _ idx tape) =
  go
    prog
    st
    ( min idx (maybe idx fst (IntMap.lookupMin tape))
    , max idx (maybe idx fst (IntMap.lookupMax tape))
    )
  where
    go :: Program a -> State a -> (Index, Index) -> (Index, Index)
    go p s res@(minIdx, maxIdx) =
      case eval p s of
        Just nextState@(State _ nidx _) -> go p nextState (min minIdx nidx, max maxIdx nidx)
        Nothing -> res

withExistentFile :: FilePath -> (Text -> IO ()) -> IO ()
withExistentFile path action = do
  exists <- doesFileExist path
  if exists
    then readFileUtf8 path >>= action
    else exitError $ "El archivo '" +| path |+ "' no existe"
