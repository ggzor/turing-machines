module Abacus.MacroSpec where

import Abacus.Macro
import MacroUtils

import Control.Monad.Except
import qualified Data.Map.Strict as M
import Test.Hspec
import Text.RawString.QQ

emptyIndex :: MacroIndex
emptyIndex = createIndexOf [r||]

testMacroIndex :: MacroIndex
testMacroIndex =
  createIndexOf
    [r|
empty_macro():

empty_macro1(t):
  t+

all_implicits():
  s:
    1+
    2- s
    x- s
    y+
    empty_macro1(t, t)
    s

with_implicits_and_params(x):
  x+
  s:
    t- s

with_single_implicit():
  t+

with_call_to_implicit():
  t+
  with_single_implicit()
|]

testMacrosInputs :: [(String, MacroName, Implicits)]
testMacrosInputs =
  [
    ( "should resolve empty macro implicits"
    , "empty_macro"
    , ([], 0)
    )
  ,
    ( "should resolve all named cell names"
    , "all_implicits"
    , (["x", "y", "t"], 0)
    )
  ,
    ( "should ignore params cell names"
    , "with_implicits_and_params"
    , (["t"], 0)
    )
  ,
    ( "should include calls to other macros"
    , "with_call_to_implicit"
    , (["t"], 1)
    )
  ]

testMacrosImplicits :: MacroMapping Implicits
testMacrosImplicits = implicitsOf testMacroIndex

spec :: Spec
spec =
  describe "resolveImplicits" $ do
    it "should resolve empty program implicits" $
      implicitsOf emptyIndex `shouldBe` M.empty
    testMacrosInputs `forM_` \(name, macroName, expected) ->
      it name $
        M.lookup macroName testMacrosImplicits
          `shouldBe` Just expected
