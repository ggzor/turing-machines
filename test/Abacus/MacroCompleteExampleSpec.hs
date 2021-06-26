module Abacus.MacroCompleteExampleSpec where

import Abacus.Macro
import MacroUtils

import Control.Monad.Except
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Test.Hspec

finalExampleInputs :: [(MacroName, Implicits)]
finalExampleInputs =
  [ ("zero", ([], 0))
  , ("trans", ([], 0))
  , ("dup", ([], 0))
  , ("copy", (["t"], 0))
  , ("sum", ([], 1))
  , ("mult", (["i", "acc"], 1))
  , ("exp", (["i", "acc"], 3))
  , ("monus", (["t"], 0))
  , ("abs_minus", (["t1", "t2"], 1))
  , ("sg", (["t"], 0))
  , ("sgn", (["s", "t"], 1))
  , ("rm", (["c", "t1", "t2", "t3"], 3))
  , ("div", (["t"], 7))
  , ("ndiv", (["c", "z"], 8))
  , ("equal", (["t"], 3))
  , ("prime", (["t", "two"], 10))
  , ("nthprime", (["c", "t"], 12))
  ]

spec :: Spec
spec =
  describe "resolveImplicits" $ do
    finalExampleInputs `forM_` \(macroName, expected) ->
      it ("should calculate correctly implicits of " ++ T.unpack macroName) $
        M.lookup macroName completeProgramImplicits
          `shouldBe` Just expected
