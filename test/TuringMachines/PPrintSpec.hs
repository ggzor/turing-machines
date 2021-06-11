module TuringMachines.PPrintSpec where

import qualified Data.Map.Strict as M
import Test.Hspec as H
import Text.RawString.QQ
import TuringMachines.Core
import TuringMachines.PPrint
import Utils
import Utils.QString

spec :: H.Spec
spec =
  describe "pprint" $ do
    it "should print empty when empty program" $
      pprint (M.empty :: Program QString) `shouldBe` ""
    it "should print correctly all types of transitions" $
      pprint
        ( M.fromList
            [ ("q1", Spec (Transition (SetTo B0) "q2") (Transition (SetTo B1) "q2"))
            , ("q2", Spec (Transition (MoveTo L) "q3") (Transition (MoveTo R) "q3"))
            ] ::
            Program QString
        )
        `shouldBe` skipFirstLine
          [r|
q1 0q2 1q2
q2 Lq3 Rq3
|]

    it "should print halt states aligned to non halt states" $
      pprint
        ( M.fromList
            [ ("q1", Spec Halt Halt)
            , ("q2", Spec (Transition (MoveTo L) "q3") (Transition (MoveTo R) "q3"))
            ] ::
            Program QString
        )
        `shouldBe` skipFirstLine
          [r|
q1  _   _
q2 Lq3 Rq3
|]

    it "should align two digit states" $
      pprint
        ( M.fromList
            [ ("q1", Spec (Transition (SetTo B0) "q10") (Transition (SetTo B1) "q10"))
            , ("q10", Spec (Transition (MoveTo L) "q3") (Transition (MoveTo R) "q3"))
            ] ::
            Program QString
        )
        `shouldBe` skipFirstLine
          [r|
q1  0q10 1q10
q10 Lq3  Rq3
|]

    it "should align two digit states and halts" $
      pprint
        ( M.fromList
            [ ("q1", Spec (Transition (SetTo B0) "q10") (Transition (SetTo B1) "q10"))
            , ("q10", Spec (Transition (MoveTo L) "q3") (Transition (MoveTo R) "q3"))
            , ("q30", Spec Halt Halt)
            ] ::
            Program QString
        )
        `shouldBe` skipFirstLine
          [r|
q1  0q10 1q10
q10 Lq3  Rq3
q30  _    _
|]

    it "should align states and specs independently" $
      pprint
        ( M.fromList
            [ ("q1", Spec (Transition (SetTo B0) "q10") (Transition (SetTo B1) "q10"))
            , ("q2", Spec (Transition (MoveTo L) "q3") (Transition (MoveTo R) "q3"))
            , ("q3", Spec Halt Halt)
            ] ::
            Program QString
        )
        `shouldBe` skipFirstLine
          [r|
q1 0q10 1q10
q2 Lq3  Rq3
q3  _    _
|]
