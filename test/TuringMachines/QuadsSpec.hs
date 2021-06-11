module TuringMachines.QuadsSpec where

import Test.Hspec
import qualified Test.Hspec as H
import TuringMachines.Core
import TuringMachines.ParseSpec (simpleProgram)
import TuringMachines.Quads
import TuringMachines.Show ()

spec :: H.Spec
spec = do
  describe "asQuads" $
    it "should return " $
      asQuads simpleProgram
        `shouldBe` [ (1, B0, SetTo B1, 1)
                   , (1, B1, SetTo B0, 2)
                   , (2, B0, MoveTo L, 2)
                   , (2, B1, MoveTo R, 3)
                   ]
