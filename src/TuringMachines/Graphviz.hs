module TuringMachines.Graphviz where

import qualified Data.Map.Strict as M
import Data.String.Interpolate
import qualified Data.Text as T
import TuringMachines.Core
import TuringMachines.Eval
import TuringMachines.Quads
import TuringMachines.Show ()

generateGraph :: Show a => Program a -> T.Text
generateGraph program =
  fillBaseTemplate $
    T.intercalate "\n"
      . fmap (T.append "  " . fmtQuad M.empty)
      . asQuads
      $ program

generateStatefulGraph :: forall a. (Show a, Eq a) => Program a -> State a -> T.Text
generateStatefulGraph program (State q idx tape) =
  let bit = readTape idx tape
      decorateChosenQuad :: Quad a -> (M.Map T.Text T.Text, Quad a)
      decorateChosenQuad quad =
        case quad of
          (src, tgtBit, _, _)
            | src == q && tgtBit == bit ->
              (M.fromAscList [("style", "bold")], quad)
          _ -> (M.empty, quad)
   in fillBaseTemplate $
        T.append
          ( T.intercalate "\n"
              . fmap (T.append "  " . uncurry fmtQuad . decorateChosenQuad)
              . asQuads
              $ program
          )
          (T.append "\n\n" [i|  #{show q} [style=filled]|])

fillBaseTemplate :: T.Text -> T.Text
fillBaseTemplate innerText =
  T.unlines . tail . T.lines $
    [i|
digraph TuringMachine {
  rankdir="LR"
  node [shape="circle"]

#{innerText}
}
|]

fmtQuad :: Show a => M.Map T.Text T.Text -> Quad a -> T.Text
fmtQuad extraAttrs (src, bit, action, tgt) =
  let joinAttr k v = [i|#{k}=#{v}|]
      extraAttrsStr =
        if M.null extraAttrs
          then T.empty
          else
            T.append ", "
              . T.intercalate ", "
              . fmap (uncurry joinAttr)
              . M.toAscList
              $ extraAttrs
   in [i|#{show src} -> #{show tgt} [label="#{bit}:#{action}"#{extraAttrsStr}]|]
