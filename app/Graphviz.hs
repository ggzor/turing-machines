module Graphviz where

import qualified Data.Text as T
import Fmt
import Text.RawString.QQ
import TuringMachines.Core
import TuringMachines.Quads
import TuringMachines.Show ()

generateGraph :: Show a => Program a -> T.Text
generateGraph program =
  fmt $
    [r|digraph TuringMachine {
  rankdir="LR"
  node [shape="circle"]

|]
      +| (T.unlines . fmap fmtQuad . asQuads $ program)
      |+ "}"

fmtQuad :: (Show a) => Quad a -> T.Text
fmtQuad (initial, bit, action, final) =
  fmt $
    "  " +| show initial |+ " -> " +| show final
      |+ " [label=\"" +| show bit
      |+ ":" +| show action
      |+ "\"]"
