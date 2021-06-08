module TuringMachines.ParseSpec where

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Test.Hspec
import qualified Test.Hspec as H
import Text.RawString.QQ
import TuringMachines.Core
import qualified TuringMachines.Parser as TP
import TuringMachines.Show ()
import Utils

simpleProgram :: Program Integer
simpleProgram =
  M.fromAscList
    [ (1, Spec (Transition (SetTo B1) 1) (Transition (SetTo B0) 2)),
      (2, Spec (Transition (MoveTo L) 2) (Transition (MoveTo R) 3))
    ]

simpleProgramText :: T.Text
simpleProgramText =
  skipFirstLine
    [r|
--  0   1
q1: 1q1 0q2
q2: Lq2 Rq3
|]

spec :: H.Spec
spec =
  describe "parse" $ do
    it "should parse simple program" $
      TP.parse simpleProgramText `shouldBe` Just simpleProgram
