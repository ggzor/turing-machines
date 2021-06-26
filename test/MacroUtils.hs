module MacroUtils where

import Abacus.Macro
import Abacus.Parser
import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text.IO as TIO
import GHC.IO.Unsafe (unsafePerformIO)
import Text.Megaparsec (parseMaybe)

completeProgramSource :: Text
{-# NOINLINE completeProgramSource #-}
completeProgramSource = unsafePerformIO $ TIO.readFile "examples/abacus.txt"

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

reifyImplicitsOf :: MacroDataMapping -> MacroIndex
reifyImplicitsOf =
  either
    (error . ("Unable to generate implicits: " ++) . show)
    id
    . runExcept
    . reifyImplicits
