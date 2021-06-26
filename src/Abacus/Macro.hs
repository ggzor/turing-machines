module Abacus.Macro where

import Abacus.Core

import Control.Monad.Except
import Control.Monad.State
import Data.Function ((&))
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Utils (tshow)

-- MacroCall is the only dirty trick here,
-- it is used with GoTo to represent a macro expansion
data MacroTag = MacroBegin | NamedTag Text | MacroCall Text [CellRef] deriving (Eq, Ord, Show)
data CellRef = CellNumber Integer | CellNamed Text deriving (Eq, Ord, Show)

data Macro = Macro Text [Text] (FlowChart MacroTag CellRef) deriving (Eq, Show)
type Program = [Macro]

type MacroName = Text

type MacroMapping = M.Map MacroName
type MacroIndex = MacroMapping Macro

type Implicits = ([Text], Integer)

data MacroError
  = UndefinedMacro MacroName
  | UnknownTag MacroName Text
  | DuplicatedTags MacroName
  deriving (Eq, Show)

indexProgram :: Program -> MacroIndex
indexProgram = M.fromList . fmap (\macro@(Macro macroName _ _) -> (macroName, macro))

type ResolveImplicitsT a = forall m. (MonadError MacroError m) => StateT (MacroMapping Implicits) m a

resolveImplicits :: (MonadError MacroError m) => MacroIndex -> m (MacroMapping Implicits)
resolveImplicits macroIndex =
  let resolveImplicitsOf :: MacroName -> ResolveImplicitsT Implicits
      resolveImplicitsOf macroName =
        lookupWithMacroName macroName macroIndex
          >>= \(Macro _ params fc) -> do
            env <- get
            newEnv <-
              M.lookup macroName env & flip maybe (const $ pure env) do
                flip (M.insert macroName) env <$> do
                  (getNamedCells fc L.\\ params,) <$> collectCallImplicits fc
            put newEnv
            lookupWithMacroName macroName newEnv
      collectCallImplicits :: FlowChart MacroTag CellRef -> ResolveImplicitsT Integer
      collectCallImplicits fc =
        (\l -> if null l then 0 else maximum l) . concat <$> do
          fc `forM` \(Seq _ nodes) ->
            nodes `forM` \case
              GoTo (MacroCall calledName _) -> do
                -- FIXME: Check for potential trampoline calls
                (tagged, imps) <- resolveImplicitsOf calledName
                pure $ fromIntegral (length tagged) + imps
              _ -> pure 0
   in mapM_ resolveImplicitsOf (M.keys macroIndex) `execStateT` M.empty

getNamedCells :: FlowChart MacroTag CellRef -> [Text]
getNamedCells fc = L.nub $ do
  (Seq _ nodes) <- fc
  nodes >>= \case
    Increase (CellNamed n) -> [n]
    Decrease (CellNamed n) _ -> [n]
    GoTo (MacroCall _ cells) ->
      concat $
        cells <&> \case
          CellNamed n -> [n]
          _ -> []
    _ -> []

lookupWithMacroName :: (MonadError MacroError m) => MacroName -> M.Map MacroName a -> m a
lookupWithMacroName macroName env =
  M.lookup macroName env & maybe (throwError (UndefinedMacro macroName)) pure

type MacroDataMapping = (MacroIndex, MacroMapping Implicits)
reifyImplicits :: (MonadError MacroError m) => MacroDataMapping -> m MacroIndex
reifyImplicits (macroIndex, macroImplicits) =
  let reifyImplicitsOf :: (MonadError MacroError m) => MacroName -> StateT MacroIndex m Macro
      reifyImplicitsOf macroName = do
        env <- get
        newEnv <-
          M.lookup macroName env & flip maybe (const $ pure env) do
            (Macro _ params fc) <- lookupWithMacroName macroName macroIndex
            (tagged, implicits) <- lookupWithMacroName macroName macroImplicits
            flip (M.insert macroName) env <$> do
              let newParams = params ++ tagged ++ map (T.append "$" . tshow) [1 .. implicits]
              Macro macroName newParams <$> rewriteCalls fc
        put newEnv
        lookupWithMacroName macroName newEnv
      rewriteCalls :: (MonadError MacroError m) => FlowChart MacroTag CellRef -> m (FlowChart MacroTag CellRef)
      rewriteCalls fc = do
        fc `forM` \(Seq begin nodes) ->
          Seq begin <$> nodes `forM` \case
            GoTo (MacroCall calledName args) -> do
              (tagged, implicits) <- lookupWithMacroName calledName macroImplicits
              let newArgs = args ++ map (CellNamed . T.append "$" . tshow) [1 .. (length tagged + fromIntegral implicits)]
              pure $ GoTo (MacroCall calledName newArgs)
            other -> pure other
   in mapM_ reifyImplicitsOf (M.keys macroIndex) `execStateT` M.empty
