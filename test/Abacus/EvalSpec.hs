module Abacus.EvalSpec where

import Abacus.Core
import Abacus.Eval
import Abacus.Macro
import Control.Monad
import Data.Function ((&))
import Data.Functor ((<&>))
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import MacroUtils
import Test.Hspec
import Text.RawString.QQ

testMacrosSource :: Text
testMacrosSource =
  T.append
    [r|

empty_program():

simple_increment():
  1+

|]
    completeProgramSource

inputs :: [(Text, IntMap Integer, IntMap Integer)]
inputs =
  [ ("empty_program", [3], [3])
  , ("simple_increment", [0], [1])
  , ("simple_increment", [], [1])
  , ("tsum_raw", [3, 2], [5, 0])
  , ("sum_raw", [3, 2, 0], [3, 5, 0])
  , ("zero", [3], [0])
  , ("trans", [3, 0], [0, 3])
  , ("trans", [3, 1], [0, 4])
  , ("dup", [3, 0, 0], [0, 3, 3])
  , ("copy", [3, 0], [3, 3])
  , ("sum", [3, 4], [3, 4, 7])
  , ("mult", [3, 4], [3, 4, 12])
  , ("exp", [2, 3], [2, 3, 8])
  , ("monus", [5, 3], [5, 3, 2])
  , ("monus", [3, 5], [3, 5, 0])
  , ("abs_minus", [5, 3], [5, 3, 2])
  , ("abs_minus", [3, 5], [3, 5, 2])
  , ("sg", [0], [0, 0])
  , ("sg", [5], [5, 1])
  , ("sg", [25], [25, 1])
  , ("sgn", [0], [0, 1])
  , ("sgn", [5], [5, 0])
  , ("sgn", [25], [25, 0])
  , ("rm", [13, 5], [13, 5, 3])
  , ("rm", [13, 4], [13, 4, 1])
  , ("rm", [81, 10], [81, 10, 1])
  , ("rm", [4, 7], [4, 7, 4])
  , ("div", [10, 5], [10, 5, 1])
  , ("div", [28, 1], [28, 1, 1])
  , ("div", [28, 2], [28, 2, 1])
  , ("div", [28, 4], [28, 4, 1])
  , ("div", [28, 7], [28, 7, 1])
  , ("div", [28, 14], [28, 14, 1])
  , ("div", [28, 28], [28, 28, 1])
  , ("div", [28, 3], [28, 3, 0])
  , ("div", [28, 5], [28, 5, 0])
  , ("div", [28, 11], [28, 11, 0])
  , ("ndiv", [28], [28, 6])
  , ("ndiv", [17], [17, 2])
  , ("equal", [0, 0], [0, 0, 1])
  , ("equal", [5, 5], [5, 5, 1])
  , ("equal", [4, 7], [4, 7, 0])
  , ("prime", [7], [7, 1])
  , ("prime", [13], [13, 1])
  , ("prime", [4], [4, 0])
  , ("prime", [28], [28, 0])
  , ("nthprime", [1], [1, 2])
  , ("nthprime", [3], [3, 5])
  , ("nthprime", [5], [5, 11])
  ]
    <&> \(n, i, o) ->
      ( n
      , cellsToMap i
      , cellsToMap o
      )
  where
    cellsToMap = IntMap.fromList . zip [1 ..]

appendZeros :: Int -> IntMap Integer -> IntMap Integer
appendZeros top m =
  let f Nothing = Just 0
      f other = other
   in [1 .. top] & foldr (IntMap.alter f) m

spec :: Spec
spec =
  describe "eval" $ do
    inputs `forM_` \(macroName, input, output) ->
      let program = M.lookup macroName testMacros & fromMaybe (error $ "Unknown program: " ++ T.unpack macroName)
       in it ("should evaluate " ++ T.unpack macroName ++ " correctly") $
            let result = eval program input
                maxReg = max (fst . IntMap.findMax $ result) (fst . IntMap.findMax $ output)
                fixedOut = appendZeros maxReg output
                fixedResult = appendZeros maxReg result
             in fixedResult `shouldBe` fixedOut

testMacros :: M.Map MacroName (FlowChart Integer Integer)
testMacros =
  let macroIndex = createIndexOf testMacrosSource
      implicits = implicitsOf macroIndex
      reified = reifyImplicitsOf (macroIndex, implicits)
   in M.mapWithKey (\name _ -> compilationOf reified name) reified
