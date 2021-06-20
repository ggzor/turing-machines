module TuringMachines.ParseSpec where

import Data.Function
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Test.Hspec
import qualified Test.Hspec as H
import Text.Megaparsec (parseMaybe)
import Text.RawString.QQ
import TuringMachines.Core
import qualified TuringMachines.Parser as TP
import TuringMachines.Show ()
import Utils

simpleProgram :: Program Integer
simpleProgram =
  M.fromAscList
    [ (1, Spec (Transition (SetTo B1) 1) (Transition (SetTo B0) 2))
    , (2, Spec (Transition (MoveTo L) 2) (Transition (MoveTo R) 3))
    ]

simpleProgramText :: T.Text
simpleProgramText =
  skipFirstLine
    [r|
--  0   1
q1 1q1 0q2
q2 Lq2 Rq3
|]

inputs :: [(String, T.Text, Maybe (Program Integer))]
inputs =
  [
    ( "should parse empty program"
    , [r|
|]
    , Just M.empty
    )
  ,
    ( "should parse simple program"
    , [r|
q1  0q1  0q1
|]
    , Just $
        M.fromAscList
          [ (1, Spec (Transition (SetTo B0) 1) (Transition (SetTo B0) 1))
          ]
    )
  ,
    ( "should parse multiline program"
    , [r|
q1  0q1  0q1
q2  0q1  0q1
|]
    , Just $
        M.fromAscList
          [ (1, Spec (Transition (SetTo B0) 1) (Transition (SetTo B0) 1))
          , (2, Spec (Transition (SetTo B0) 1) (Transition (SetTo B0) 1))
          ]
    )
  ,
    ( "should parse multiline program with line comments"
    , [r|
--   0    1
q1  0q1  0q1
q2  0q1  0q1

-- Thats a program
|]
    , Just $
        M.fromAscList
          [ (1, Spec (Transition (SetTo B0) 1) (Transition (SetTo B0) 1))
          , (2, Spec (Transition (SetTo B0) 1) (Transition (SetTo B0) 1))
          ]
    )
  ,
    ( "should reject multiple specs in the same line"
    , [r|
q1  0q1  0q1 q2  0q1  0q1
|]
    , Nothing
    )
  ,
    ( "should parse all possible actions"
    , [r|
q1  0q1  1q1
q2  Lq1  Rq1
|]
    , Just $
        M.fromAscList
          [ (1, Spec (Transition (SetTo B0) 1) (Transition (SetTo B1) 1))
          , (2, Spec (Transition (MoveTo L) 1) (Transition (MoveTo R) 1))
          ]
    )
  ]

spec :: H.Spec
spec =
  describe "parse" $
    inputs & mapM_ \(message, input, result) ->
      it message $ parseMaybe TP.pProgram (skipFirstLine input) `shouldBe` result
