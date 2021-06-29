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
  , ("third_register_decrement", fromNats [1, 2, 3, 4, 5], fromNats [1, 2, 2, 4, 5, 0])
  ,
    ( "high_numbered_register_decrement"
    , fromNats $ replicate 15 7
    , fromNats $ replicate 9 7 ++ [6] ++ replicate 5 7 ++ [0]
    )
  ]
  where
    fromNats :: [Int] -> Tape
    fromNats =
      IntMap.fromAscList
        . zip [0 ..]
        . L.intercalate [B0]
        . map \x -> replicate (x + 1) B1
    emptyTape = IntMap.singleton 0 B0
    tape :: Text -> Tape
    tape str =
      str & parseMaybe pTape
        & fromMaybe (error $ "Wrong tape: " ++ T.unpack str)

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
                    minIdx = min (fst $ IntMap.findMin result) (fst $ IntMap.findMin output)
                    maxIdx = max (fst $ IntMap.findMax result) (fst $ IntMap.findMax output)
                    adjustedResult = tapeSlice minIdx maxIdx result
                    adjustedOutput = tapeSlice minIdx maxIdx output
                 in adjustedResult `shouldBe` adjustedOutput

tapeSlice :: Int -> Int -> Tape -> String
tapeSlice minIdx maxIdx tape = bitToChar . flip readTape tape <$> [minIdx .. maxIdx]

bitToChar :: Bit -> Char
bitToChar = \case
  B0 -> '0'
  B1 -> '1'
