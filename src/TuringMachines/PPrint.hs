module TuringMachines.PPrint where

import qualified Data.Map.Strict as M
import Data.Text as T
import TuringMachines.Core
import TuringMachines.Show ()

pprint :: Show a => Program a -> Text
pprint = T.unlines . fmap (\(q, spec) -> T.unwords [pack (show q), pprintSpec spec]) . M.toAscList

pprintSpec :: Show a => Spec a -> Text
pprintSpec (Spec if0 if1) = T.unwords [pprintTransition if0, pprintTransition if1]

pprintTransition :: Show a => Transition a -> Text
pprintTransition (Transition a q) = T.concat [pprintAction a, pack (show q)]
pprintTransition Halt = "_"

pprintAction :: Action -> Text
pprintAction (SetTo B0) = "0"
pprintAction (SetTo B1) = "1"
pprintAction (MoveTo L) = "L"
pprintAction (MoveTo R) = "R"

pprintBit :: Bit -> Text
pprintBit b = case b of
  B0 -> "0"
  B1 -> "1"
