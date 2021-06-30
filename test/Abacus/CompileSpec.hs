module Abacus.CompileSpec where

import Abacus.Macro
import TuringMachines.Core
import TuringMachines.Show ()

import qualified Abacus.Compiler as AB
import Control.Monad
import Data.Function ((&))
import qualified Data.IntMap as IntMap
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.String.Interpolate
import Data.Text (Text)
import qualified Data.Text as T
import MacroUtils
import Test.Hspec
import qualified Test.Hspec as H
import Text.Megaparsec (parseMaybe)
import Text.RawString.QQ
import qualified TuringMachines.Core as TM
import TuringMachines.Eval
import TuringMachines.Parser

testMacrosSource :: Text
testMacrosSource =
  T.append
    completeProgramSource
    [r|

empty_program():

simple_increment():
  1+

third_register_increment():
  3+

high_numbered_register():
  10+

simple_decrement():
  1- e
  e:

third_register_decrement():
  3- e
  e:

high_numbered_register_decrement():
  10- e
  e:

trans1to3():
  trans(1, 3)

dup3to1and4():
  dup(3, 1, 4)

|]

compiledTestMacros :: M.Map MacroName (TM.Program Integer)
compiledTestMacros =
  fromMaybe
    (error "Unable to compile macros")
    (traverse AB.compile $ compiledMacrosOf testMacrosSource)

inputs :: [(MacroName, Tape, Tape)]
inputs =
  [ ("empty_program", tape "0", tape "0")
  , ("simple_increment", tape "10", tape "11")
  , ("simple_increment", tape "1110", tape "1111")
  , ("third_register_increment", tape "10101", tape "101011")
  , ("third_register_increment", tape "11011101111", tape "110111011111")
  , -- Normalization of zero registers
    ("third_register_increment", tape "000000", tape "101011")
  , ("high_numbered_register", emptyTape, fromNats $ replicate 9 0 ++ [1])
  , ("high_numbered_register", fromNats [1 .. 10], fromNats $ [1 .. 9] ++ [11])
  , ("simple_decrement", tape "00", tape "10")
  , ("simple_decrement", tape "1100", tape "101")
  , ("simple_decrement", tape "111100", tape "11101")
  , ("third_register_decrement", tape "10101100", tape "1010101")
  , -- Extra zeros are added because of B-repr to 1-repr conversion
    ("third_register_decrement", fromNats [1, 2, 3, 4, 5], fromNats [1, 2, 2, 4, 5])
  ,
    ( "high_numbered_register_decrement"
    , fromNats $ replicate 15 7
    , fromNats $ replicate 9 7 ++ [6] ++ replicate 5 7
    )
  , ("trans1to3", fromNats [1, 0, 0], fromNats [0, 0, 1])
  , ("trans1to3", fromNats [3, 2, 0], fromNats [0, 2, 3])
  , ("dup3to1and4", fromNats [0, 0, 1, 0], fromNats [1, 0, 0, 1])
  , ("dup3to1and4", fromNats [0, 2, 3, 0, 4], fromNats [3, 2, 0, 3, 4])
  , ("sum", fromNats [1, 2, 0], fromNats [1, 2, 3])
  ]
  where
    emptyTape = IntMap.singleton 0 B0
    tape :: Text -> Tape
    tape str =
      str & parseMaybe pTape
        & fromMaybe (error $ "Wrong tape: " ++ T.unpack str)

fromNats :: [Int] -> Tape
fromNats =
  IntMap.fromAscList
    . zip [0 ..]
    . L.intercalate [B0]
    . map \x -> replicate (x + 1) B1

toNats :: Tape -> [Int]
toNats tape =
  let (minIdx, maxIdx) = tapeBounds tape
   in L.dropWhileEnd (== 0)
        . map ((+ (-1)) . length)
        . filter \case
          '1' : _ -> True
          _ -> False
        . L.group
        . tapeSlice minIdx maxIdx
        $ tape

tapeBounds :: Tape -> (Int, Int)
tapeBounds tape =
  if IntMap.null tape
    then (0, 0)
    else (fst $ IntMap.findMin tape, fst $ IntMap.findMax tape)

spec :: H.Spec
spec =
  describe "compile" $
    inputs `forM_` \(macroName, input, output) ->
      let program =
            fromMaybe (error $ "Unknown macro: " ++ T.unpack macroName) $
              M.lookup macroName compiledTestMacros
          adjustedInput = tapeSlice 0 (fst $ IntMap.findMax input) input
       in do
            it
              [__i|
              should run correctly #{macroName} with input "#{adjustedInput}"
            |]
              $ let (State _ _ result) = evalFull program (State 1 0 input)
                    normResult = fromNats . toNats $ result
                    normOutput = fromNats . toNats $ output
                    (minResult, maxResult) = tapeBounds normResult
                    (minOutput, maxOutput) = tapeBounds normOutput
                    minIdx = min minResult minOutput
                    maxIdx = max maxResult maxOutput
                    adjustedResult = tapeSlice minIdx maxIdx normResult
                    adjustedOutput = tapeSlice minIdx maxIdx normOutput
                 in adjustedResult `shouldBe` adjustedOutput

tapeSlice :: Int -> Int -> Tape -> String
tapeSlice minIdx maxIdx tape = bitToChar . flip readTape tape <$> [minIdx .. maxIdx]

bitToChar :: Bit -> Char
bitToChar = \case
  B0 -> '0'
  B1 -> '1'
