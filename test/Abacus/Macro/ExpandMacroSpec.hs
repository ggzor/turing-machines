module Abacus.Macro.ExpandMacroSpec where

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

call_to_empty_macro():
  empty_macro()

simple_arg_macro(x):
  x+

call_to_simple_arg_macro(x, y):
  simple_arg_macro(y)

call_with_extra_instructions(x, y):
  x+
  simple_arg_macro(y)

nested_macro_call(x, y):
  call_to_simple_arg_macro(y, x)

tagged_macro(x, y):
  s:
    x- e
    y+
    s
  e:

call_to_tagged_macro(a1, a2):
  tagged_macro(a1, a2)
  tagged_macro(a2, a1)

call_to_nested_tagged_macro(x, y):
  s:
    x- e
    call_to_tagged_macro(x, y)
    call_to_tagged_macro(y, x)
    s
  e:

|]

rewrittenMacroIndex :: MacroIndex
rewrittenMacroIndex =
  createIndexOf
    [r|

empty_macro():

call_to_empty_macro():

simple_arg_macro(x):
  x+

call_to_simple_arg_macro(x, y):
  y+

call_with_extra_instructions(x, y):
  x+
  y+

nested_macro_call(x, y):
  x+

tagged_macro(x, y):
  s:
    x- e
    y+
    s
  e:

call_to_tagged_macro(a1, a2):
  s.0:
    a1- e.0
    a2+
    s.0
  e.0:
  s.1:
    a2- e.1
    a1+
    s.1
  e.1:

call_to_nested_tagged_macro(x, y):
  s:
    x- e
    -- Tag 0 and 3 are used when expanding call_to_tagged_macro
    s.1:
      x- e.1
      y+
      s.1
    e.1:
    s.2:
      y- e.2
      x+
      s.2
    e.2:
    s.4:
      y- e.4
      x+
      s.4
    e.4:
    s.5:
      x- e.5
      y+
      s.5
    e.5:
    s
  e:

|]

spec :: Spec
spec =
  describe "expandMacro" $ do
    M.keys testMacroIndex `forM_` \macroName -> do
      it ("should expand macro " ++ T.unpack macroName ++ " correctly") $
        fromMaybe (error $ "Macro not found: " ++ T.unpack macroName) $
          shouldBe
            <$> pure (macroExpansionOf testMacroIndex macroName)
            <*> M.lookup macroName rewrittenMacroIndex
