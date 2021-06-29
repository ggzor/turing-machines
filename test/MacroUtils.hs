module MacroUtils where

import Abacus.Core
import Abacus.Macro
import Abacus.Parser
import Control.Monad.Except
import Control.Monad.State
import Data.Function ((&))
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
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

macroExpansionOf :: MacroIndex -> MacroName -> Macro
macroExpansionOf macroIndex macroName =
  let (Macro _ params fc) =
        fromMaybe (error $ "Macro not found in index: " ++ T.unpack macroName) $
          M.lookup macroName macroIndex
   in fc & expandMacros macroIndex
        & (`evalStateT` TagGen M.empty)
        & runExcept
        & either
          (error . (("Unable to expand macro " ++ T.unpack macroName) ++) . show)
          (Macro macroName params)

compilationOf :: MacroIndex -> MacroName -> FlowChart Integer Integer
compilationOf macroIndex macroName =
  compile macroIndex macroName
    & runExcept
    & either
      (error . (("Unable to expand macro " ++ T.unpack macroName) ++) . show)
      id

compiledMacrosOf :: Text -> M.Map MacroName (FlowChart Integer Integer)
compiledMacrosOf macrosSource =
  let macroIndex = createIndexOf macrosSource
      implicits = implicitsOf macroIndex
      reified = reifyImplicitsOf (macroIndex, implicits)
   in M.mapWithKey (\name _ -> compilationOf reified name) reified
