module MacroUtils where

import Abacus.Macro
import Abacus.Parser
import Control.Monad.Except
import Data.Text (Text)
import Text.Megaparsec (parseMaybe)
import Text.RawString.QQ

completeProgramSource :: Text
completeProgramSource =
  [r|
zero(x):
  s:
    x- e
    s
  e:

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

copy(x, y):
  dup(x, y, t)
  trans(t, x)

sum(x, y, r):
  copy(x, r)
  copy(y, r)

mult(x, y, r):
  copy(x, i)
  s:
    i- e
    sum(y, r, acc)
    trans(acc, r)
    s
  e:

monus(x, y, r):
  trans(x, r)
  trans(y, t)
  s:
    t- e
    r- e
    s
  e:
    zero(t)

abs_minus(x, y, r):
  monus(x, y, t1)
  monus(y, x, t2)
  sum(t1, t2, r)

sg(x, r):
  trans(x, t)
  t- e
  r+
  e:
    zero(t)

sgn(x, r):
  sg(x, s)
  t+
  monus(t, s, r)
  zero(t)
  zero(s)

rm(x, y, r):
  copy(y, c)
  l:
    c- e
    r+
    abs_minus(x, r, t1)
    sg(t1, t2)
    mult(r, t2, t3)
    trans(t3, r)

div(x, y, r):
  rm(x, y, t)
  sgn(t, r)
  zero(t)

|]

completeProgramIndex :: MacroIndex
completeProgramIndex = createIndexOf completeProgramSource

completeProgramImplicits :: MacroMapping Implicits
completeProgramImplicits = implicitsOf completeProgramIndex

createIndexOf :: Text -> MacroIndex
createIndexOf =
  maybe (error "Invalid program in test") indexProgram . parseMaybe pProgram

implicitsOf :: MacroIndex -> MacroMapping Implicits
implicitsOf =
  either
    (error . ("Unable to generate implicits: " ++) . show)
    id
    . runExcept
    . resolveImplicits
