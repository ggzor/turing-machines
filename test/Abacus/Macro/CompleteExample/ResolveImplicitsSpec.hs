module Abacus.Macro.CompleteExample.ResolveImplicitsSpec where

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
  , ("mult", (["i"], 1))
  , ("exp", (["i", "acc"], 2))
  , ("monus", (["t"], 1))
  , ("abs_minus", (["t1", "t2"], 2))
  , ("sg", ([], 0))
  , ("sgn", (["s", "t"], 2))
  , ("rm", (["c", "t1", "t2", "t3"], 4))
  , ("div", (["t"], 8))
  , ("ndiv", (["c", "z"], 9))
  , ("equal", (["t"], 4))
  , ("prime", (["t", "two"], 11))
  , ("nthprime", (["c", "t"], 13))
  ]

spec :: Spec
spec =
  describe "resolveImplicits" $ do
    finalExampleInputs `forM_` \(macroName, expected) ->
      it ("should calculate correctly implicits of " ++ T.unpack macroName) $
        M.lookup macroName completeProgramImplicits
          `shouldBe` Just expected
