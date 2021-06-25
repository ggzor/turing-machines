module Abacus.ParserSpec where

import Abacus.Parser

import Abacus.Core
import Data.Function ((&))
import Data.Text (Text)
import Test.Hspec
import Text.Megaparsec (parseMaybe)
import Text.RawString.QQ
import Utils

inputs :: [(String, Text, Maybe Program)]
inputs =
  [
    ( "should parse empty program"
    , skipFirstLine
        [r|
|]
    , Just []
    )
  ,
    ( "should ignore all types of comments and space"
    , skipFirstLine
        [r|
-- Single line comment

{-
Multiline
Comment
Here
-}
|]
    , Just []
    )
  ,
    ( "should parse the empty macro"
    , "test():"
    , Just [Macro "test" [] []]
    )
  ,
    ( "should parse many empty macros separated by newline"
    , [r|
test():
test2():
|]
    , Just [Macro "test" [] [], Macro "test2" [] []]
    )
  ,
    ( "should not parse macros not separated by newline"
    , [r|
test(): test2():
|]
    , Nothing
    )
  ,
    ( "should parse the empty macro with params"
    , [r|
test(x, y):
|]
    , Just [Macro "test" ["x", "y"] []]
    )
  ,
    ( "should parse increment instructions with cell numbers"
    , [r|
suc():
  1+
|]
    , Just [Macro "suc" [] [Seq MacroBegin [Increase (CellNumber 1)] MacroEnd]]
    )
  ,
    ( "should not parse same line instructions"
    , [r|
suc(): 1+
|]
    , Nothing
    )
  ,
    ( "should parse multiple instructions"
    , [r|
suc():
  1+
  2+
|]
    , Just
        [ Macro
            "suc"
            []
            [ Seq
                MacroBegin
                [ Increase (CellNumber 1)
                , Increase (CellNumber 2)
                ]
                MacroEnd
            ]
        ]
    )
  ,
    ( "should parse named cells"
    , [r|
suc(x):
  x+
|]
    , Just
        [ Macro
            "suc"
            ["x"]
            [ Seq
                MacroBegin
                [ Increase (CellNamed "x")
                ]
                MacroEnd
            ]
        ]
    )
  ,
    ( "should parse named tag"
    , [r|
suc(x):
  s:
    x+
|]
    , Just
        [ Macro
            "suc"
            ["x"]
            [ Seq
                (NamedTag "s")
                [ Increase (CellNamed "x")
                ]
                MacroEnd
            ]
        ]
    )
  ,
    ( "shoud not parse instruction in named tag line"
    , [r|
suc(x):
  s: x+
|]
    , Nothing
    )
  ,
    ( "should parse decrease instructions"
    , [r|
sg(x, r):
  s:
    x- e
    r+
  e:
|]
    , Just
        [ Macro
            "sg"
            ["x", "r"]
            [ Seq
                (NamedTag "s")
                [ Decrease (CellNamed "x") (NamedTag "e")
                , Increase (CellNamed "r")
                ]
                (NamedTag "e")
            , Seq
                (NamedTag "e")
                []
                MacroEnd
            ]
        ]
    )
  ,
    ( "should parse goto instructions"
    , [r|
trans(x, y):
  s:
    x- e
    y+
    s
  e:
|]
    , Just
        [ Macro
            "trans"
            ["x", "y"]
            [ Seq
                (NamedTag "s")
                [ Decrease (CellNamed "x") (NamedTag "e")
                , Increase (CellNamed "y")
                , GoTo (NamedTag "s")
                ]
                (NamedTag "e")
            , Seq
                (NamedTag "e")
                []
                MacroEnd
            ]
        ]
    )
  ,
    ( "should parse macro calls"
    , [r|
sg(x, r):
  trans(x, t)
  t- e
  r+
  e:
    zero(t)
|]
    , Just
        [ Macro
            "sg"
            ["x", "r"]
            [ Seq
                MacroBegin
                [ GoTo (MacroCall "trans" ["x", "t"])
                , Decrease (CellNamed "t") (NamedTag "e")
                , Increase (CellNamed "r")
                ]
                (NamedTag "e")
            , Seq
                (NamedTag "e")
                [ GoTo (MacroCall "zero" ["t"])
                ]
                MacroEnd
            ]
        ]
    )
  ]

spec :: Spec
spec =
  describe "parse" $ do
    inputs & mapM_ \(message, input, result) ->
      it message $
        parseMaybe pProgram input `shouldBe` result
