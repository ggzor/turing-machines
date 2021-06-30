module Commands.Abacus.Runner where

import Commands.Abacus.Parser

import qualified Abacus.Compiler as ABCompiler
import Abacus.Macro
import qualified Abacus.Macro as ABMacro
import Abacus.Parser
import Data.Bifunctor
import Data.String.Interpolate
import Data.Text
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import qualified TuringMachines.Core as TM
import Utils

processAbacus :: Text -> AbacusSubCommandOptions -> IO ()
processAbacus macroIndexSource (Compile macroName) =
  either processAbacusError (putStrLn . programAsStr) $
    compileMacro macroIndexSource (T.pack macroName)

processAbacusError :: ProcessAbacusError -> IO ()
processAbacusError (ParserError errorBundle) = putStrLn $ errorBundlePretty errorBundle
processAbacusError (MacroProcessingError (UndefinedMacro name)) =
  exitError [i|The macro '#{name}' is referenced but is not defined.|]
processAbacusError (MacroProcessingError (UnknownTag macroName tagName)) =
  exitError [i|The tag '#{tagName}' is referenced but not defined within the macro '#{macroName}'.|]
processAbacusError (MacroProcessingError (DuplicatedTags macroName)) =
  exitError [i|There are duplicated tags within the macro '#{macroName}'.|]
processAbacusError TuringTranslationError =
  exitError "There was a problem translating the macro to a Turing machine."

data ProcessAbacusError
  = ParserError (ParseErrorBundle Text Void)
  | MacroProcessingError MacroError
  | TuringTranslationError

compileMacro :: Text -> MacroName -> Either ProcessAbacusError (TM.Program Integer)
compileMacro macrosSource macroName = do
  macroProgram <- first ParserError $ runParser pProgram "" macrosSource
  let macroIndex = indexProgram macroProgram
  implicits <- first MacroProcessingError $ resolveImplicits macroIndex
  reifiedMacros <- first MacroProcessingError $ reifyImplicits (macroIndex, implicits)
  compiledMacro <- first MacroProcessingError $ ABMacro.compile reifiedMacros macroName
  maybe (Left TuringTranslationError) Right $ ABCompiler.compile compiledMacro
