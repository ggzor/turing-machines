module TuringMachines.EvalSpec where

import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Test.Hspec as H
import TuringMachines.Core
import TuringMachines.Eval
import TuringMachines.Show ()

spec :: H.Spec
spec = do
  describe "eval" $ do
    it "evaluates empty machine halting" $ do
      eval emptyMachine emptyState `shouldBe` Nothing

emptyMachine :: Program Int
emptyMachine = M.empty

emptyState :: State Int
emptyState = State 1 1 IM.empty
