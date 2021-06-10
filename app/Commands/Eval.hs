module Commands.Eval where

import Control.Monad (void)
import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Fmt (fmt, padRightF, (+|), (|+))
import Parser
import SVG
import System.Console.ANSI
import System.IO (hFlush, stdout)
import TuringMachines.Core
import TuringMachines.Eval (eval, readTape)

processEval :: Integer -> EvalOptions -> Program Integer -> State Integer -> IO ()
processEval currentSteps opts@EvalOptions {lineByLine, limitSteps} program state = do
  pprintState program state
  printImage program state
  if lineByLine
    then hFlush stdout >> void getLine
    else putStrLn ""
  if maybe False (currentSteps >=) limitSteps
    then pure ()
    else do
      let newState = eval program state
      case newState of
        Nothing -> pure ()
        Just st ->
          processEval
            (currentSteps + 1)
            opts
            program
            st

pprintState :: Program Integer -> State Integer -> IO ()
pprintState program (State q idx tape) = do
  let maxStateLength = length . show $ maybe 0 fst (M.lookupMax program)
      minIdx = maybe idx fst (IntMap.lookupMin tape)
      maxIdx = maybe idx fst (IntMap.lookupMax tape)
  fmt $ padRightF (maxStateLength + 3) ' ' (("<q" +| q |+ ">") :: Text) |+ " " +| tapeInterval minIdx idx |+ ""
  setSGR [SetUnderlining SingleUnderline, SetConsoleIntensity BoldIntensity, SetColor Foreground Vivid Red]
  putChar . bitToStr $ readTape idx tape
  setSGR [Reset]
  putStr $ tapeInterval (idx + 1) (maxIdx + 1)
  where
    tapeInterval :: Index -> Index -> String
    tapeInterval start end = bitToStr . flip readTape tape <$> [start .. end - 1]

bitToStr :: Bit -> Char
bitToStr b = case b of
  B0 -> '0'
  B1 -> '1'
