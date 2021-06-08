module TuringMachines.GraphvizSpec where

import Data.Maybe (fromMaybe)
import Test.Hspec
import qualified Test.Hspec as H
import Text.RawString.QQ
import TuringMachines.Core
import TuringMachines.Graphviz
import qualified TuringMachines.Parser as TP
import Utils

simpleProgram :: Program Integer
simpleProgram =
  fromMaybe undefined . TP.parse . skipFirstLine $
    [r|
--  0   1
q1: 1q1 0q2
q2: Lq2 Rq3
|]

spec :: H.Spec
spec = do
  describe "generateGraph" $
    it "should generate simple graph for program" $
      generateGraph simpleProgram
        `shouldBe` skipFirstLastLine
          [r|
digraph TuringMachine {
  rankdir="LR"
  node [shape="circle"]

  1 -> 1 [label="0:1"]
  1 -> 2 [label="1:0"]
  2 -> 2 [label="0:L"]
  2 -> 3 [label="1:R"]
}
|]
