module TuringMachines.GraphvizSpec where

import qualified Data.IntMap as IntMap
import Test.Hspec
import qualified Test.Hspec as H
import Text.RawString.QQ
import TuringMachines.Core
import TuringMachines.Graphviz
import TuringMachines.ParseSpec (simpleProgram)
import Utils

spec :: H.Spec
spec = do
  describe "generateGraph" $ do
    it "should generate simple graph for program" $
      generateGraph simpleProgram
        `shouldBe` skipFirstLine
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
    it "should generate stateful graphs for programs" $
      generateStatefulGraph
        simpleProgram
        (State 2 0 (IntMap.fromAscList [(0, B0)]))
        `shouldBe` skipFirstLine
          [r|
digraph TuringMachine {
  rankdir="LR"
  node [shape="circle"]

  1 -> 1 [label="0:1"]
  1 -> 2 [label="1:0"]
  2 -> 2 [label="0:L", style=bold, color=red4]
  2 -> 3 [label="1:R"]

  2 [style=filled]
}
|]
