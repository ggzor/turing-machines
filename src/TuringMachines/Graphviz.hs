module TuringMachines.Graphviz where

import Data.String.Interpolate
import qualified Data.Text as T
import TuringMachines.Core
import TuringMachines.Quads
import TuringMachines.Show ()

generateGraph :: Show a => Program a -> T.Text
generateGraph program =
  T.unlines . init . tail . T.lines $
    [i|
digraph TuringMachine {
  rankdir="LR"
  node [shape="circle"]

#{T.intercalate "\n" . fmap (T.append "  " . fmtQuad) . asQuads $ program}
}
|]

fmtQuad :: Show a => Quad a -> T.Text
fmtQuad (src, bit, action, tgt) =
  [i|#{show src} -> #{show tgt} [label="#{bit}:#{action}"]|]
