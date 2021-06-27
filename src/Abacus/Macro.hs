module Abacus.Macro where

import Abacus.Core

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Data.Either (isLeft, lefts, rights)
import Data.Function ((&))
import Data.Functor ((<&>))
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Utils (tshow)
import Data.Traversable (for)

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

newtype TagGen a = TagGen {unTagGen :: M.Map a Integer}

class TagGenFromInt a b where
  genTag :: a -> Integer -> b

fresh :: (Ord a, TagGenFromInt a b, MonadState (TagGen a) m) => a -> m b
fresh tag = do
  TagGen m <- get
  let newMap =
        m & flip M.alter tag \case
          Nothing -> Just 0
          (Just v) -> Just (v + 1)
  put (TagGen newMap)
  pure . genTag tag . fromMaybe 0 $ M.lookup tag newMap

data ExpandTag = TempTag | ScopedTag deriving (Eq, Ord)
instance TagGenFromInt ExpandTag Text where
  genTag TempTag = T.append "$" . tshow
  genTag ScopedTag = T.append "." . tshow

expandMacros ::
  (MonadError MacroError m, MonadState (TagGen ExpandTag) m) =>
  MacroIndex ->
  FlowChart MacroTag CellRef ->
  m (FlowChart MacroTag CellRef)
expandMacros macroIndex fc =
  concat <$> fc `forM` \(Seq begin nodes) -> do
    result <-
      nodes `forM` \case
        GoTo (MacroCall calledName args) -> do
          scope <- fresh ScopedTag
          calledMacro <- lookupWithMacroName calledName macroIndex
          Right <$> expandMacros macroIndex (retagWithScope scope $ instantiateMacro calledMacro args)
        other -> pure . Left $ other

    mergedResult <-
      result
        & L.groupBy (\a b -> isLeft a && isLeft b)
        & mapM \l -> case l of
          [] -> pure []
          Left _ : _ -> do
            tag <- NamedTag <$> fresh TempTag
            pure [Seq tag $ lefts l]
          Right _ : _ ->
            pure . concat $ rights l

    pure . compactSeqs $ Seq begin [] : concat mergedResult

retagWithScope :: Text -> FlowChart MacroTag CellRef -> FlowChart MacroTag CellRef
retagWithScope scope fc =
  let addScope tag = NamedTag $ T.append tag scope
      retagNode (GoTo (NamedTag tag)) = GoTo (addScope tag)
      retagNode (Decrease cr (NamedTag tag)) = Decrease cr (addScope tag)
      retagNode other = other
   in fc <&> \case
        Seq (NamedTag tag) nodes -> Seq (addScope tag) $ fmap retagNode nodes
        Seq tag nodes -> Seq tag $ fmap retagNode nodes

-- FIXME: The slowest function here, but maybe not the most used
compactSeqs :: FlowChart MacroTag CellRef -> FlowChart MacroTag CellRef
compactSeqs =
  let addSeq [] (Seq MacroBegin []) = []
      addSeq [] (Seq tag nodes) | isTempTag tag = [Seq MacroBegin nodes]
      addSeq [] other = [other]
      addSeq (Seq prevTag prevNodes : rest) (Seq newTag nodes)
        | isTempTag newTag =
          Seq prevTag (prevNodes ++ nodes) : rest
      addSeq rest new = new : rest
   in reverse . foldr (flip addSeq) [] . reverse

isTempTag :: MacroTag -> Bool
isTempTag (NamedTag name) = "$" `T.isPrefixOf` name
isTempTag MacroBegin = True
isTempTag _ = False

instantiateMacro :: Macro -> [CellRef] -> FlowChart MacroTag CellRef
instantiateMacro (Macro _ params fc) args =
  let env = M.fromList $ zip (CellNamed <$> params) args
   in renameCellRefs env fc

renameCellRefs :: M.Map CellRef CellRef -> FlowChart MacroTag CellRef -> FlowChart MacroTag CellRef
renameCellRefs renamings =
  let replaceIfNecessary cr = fromMaybe cr $ M.lookup cr renamings
      mapCellRefs = \case
        Increase cr -> Increase $ replaceIfNecessary cr
        Decrease cr tag -> Decrease (replaceIfNecessary cr) tag
        GoTo (MacroCall calledName args) -> GoTo . MacroCall calledName $ replaceIfNecessary <$> args
        other -> other
   in runIdentity . traverseNodes (Identity . mapCellRefs)

traverseNodes :: Applicative m => (Node tag a -> m (Node tag b)) -> FlowChart tag a -> m (FlowChart tag b)
traverseNodes f fc =
  fc `for` \(Seq begin nodes) ->
    Seq begin <$> nodes `for` f
