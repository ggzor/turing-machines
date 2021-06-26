module Abacus.Macro.ReifyImplicitsSpec where

import Abacus.Macro
import MacroUtils

import Control.Monad.Except
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import Test.Hspec
import Text.RawString.QQ

testMacroIndex :: MacroIndex
testMacroIndex =
  createIndexOf
    [r|

empty_macro():

single_tagged():
  x+

single_tagged_with_params(y):
  x+

with_call_to_implicit():
  single_tagged()

with_call_to_implicit_with_params(y):
  single_tagged_with_params(x)

with_nested_call_to_implicits():
  with_call_to_implicit()

with_nested_call_to_implicits_with_params(y):
  with_call_to_implicit_with_params(x)

reuses_implicits(y):
  with_call_to_implicit_with_params(x)
  with_call_to_implicit_with_params(x)
  with_call_to_implicit()

|]

rewrittenMacroIndex :: MacroIndex
rewrittenMacroIndex =
  createIndexOf
    [r|

empty_macro():

single_tagged(x):
  x+

single_tagged_with_params(y, x):
  x+

with_call_to_implicit($1):
  single_tagged($1)

with_call_to_implicit_with_params(y, x, $1):
  single_tagged_with_params(x, $1)

with_nested_call_to_implicits($1):
  with_call_to_implicit($1)

with_nested_call_to_implicits_with_params(y, x, $1, $2):
  with_call_to_implicit_with_params(x, $1, $2)

reuses_implicits(y, x, $1, $2):
  with_call_to_implicit_with_params(x, $1, $2)
  with_call_to_implicit_with_params(x, $1, $2)
  with_call_to_implicit($1)

|]

reifiedTestMacros :: MacroIndex
reifiedTestMacros = reifyImplicitsOf (testMacroIndex, implicitsOf testMacroIndex)

reifiedTestMacrosImplicits :: MacroMapping Implicits
reifiedTestMacrosImplicits = implicitsOf reifiedTestMacros

spec :: Spec
spec =
  describe "reifyImplicits" $ do
    M.keys testMacroIndex `forM_` \macroName -> do
      it ("should reify implicits of" ++ T.unpack macroName) $
        M.lookup macroName reifiedTestMacrosImplicits
          `shouldBe` Just ([], 0)
      it ("should rewrite " ++ T.unpack macroName ++ " without implicits") $
        fromMaybe (error $ "Macro not found: " ++ T.unpack macroName) $
          shouldBe
            <$> M.lookup macroName reifiedTestMacros
            <*> M.lookup macroName rewrittenMacroIndex
