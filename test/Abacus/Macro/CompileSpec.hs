module Abacus.Macro.CompileSpec where

import Abacus.Macro
import MacroUtils

import Abacus.Core
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

trans(x, y):
  s:
    x- e
    y+
    s
  e:

dup(x, y, z):
  s:
    x- e
    y+
    z+
    s
  e:

copy(x, y, i1):
  dup(x, y, i1)
  trans(i1, x)

sum(x, y, r, i1):
  copy(x, r, i1)
  copy(y, r, i1)

|]

rewrittenMacroIndex :: MacroIndex
rewrittenMacroIndex =
  createIndexOf
    [r|

empty_macro():

trans():
  $0:
    1- $1
    2+
    $0
  $1:

dup():
  $0:
    1- $1
    2+
    3+
    $0
  $1:

copy():
  $0:
    1- $1
    2+
    3+
    $0
  $1:
  $2:
    3- $3
    1+
    $2
  $3:

sum():
  $0:
    1- $1
    3+
    4+
    $0
  $1:
  $2:
    4- $3
    1+
    $2
  $3:
  $4:
    2- $5
    3+
    4+
    $4
  $5:
  $6:
    4- $7
    2+
    $6
  $7:

|]

spec :: Spec
spec =
  describe "compile" $ do
    M.keys testMacroIndex `forM_` \macroName -> do
      it ("should compile macro " ++ T.unpack macroName ++ " correctly") $
        fromMaybe (error $ "Macro not found: " ++ T.unpack macroName) $
          shouldBe
            <$> pure (compilationOf testMacroIndex macroName)
            <*> (prepareMacro =<< M.lookup macroName rewrittenMacroIndex)

prepareMacro :: Macro -> Maybe (FlowChart Integer Integer)
prepareMacro (Macro _ _ fc) = enumerateTags =<< unwrapNumbered fc
