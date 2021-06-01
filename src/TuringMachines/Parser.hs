module TuringMachines.Parser where

import Control.Monad (guard)
import qualified Data.Char as C
import qualified Data.IntMap.Strict as IntMap
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import TuringMachines.Core
import Prelude hiding (lines)

parse :: T.Text -> Maybe (Program Integer)
parse code =
  let lines =
        map (T.words . T.strip)
          . filter (not . T.null)
          . filter (not . T.isPrefixOf "--")
          . map T.strip
          $ T.lines code
   in L.foldl' (flip (uncurry M.insert)) M.empty <$> traverse parseSpec lines

parseStateName :: T.Text -> Maybe Integer
parseStateName name = do
  guard (T.length name > 0)
  guard (T.all C.isAlphaNum name)
  pure $ read . T.unpack . T.filter C.isNumber $ name

parseSpec :: [T.Text] -> Maybe (Integer, Spec Integer)
parseSpec [state, t0, t1] = do
  name <- T.stripSuffix ":" state >>= parseStateName
  tp0 <- parseTransition t0
  tp1 <- parseTransition t1
  pure (name, Spec tp0 tp1)
parseSpec _ = Nothing

parseTransition :: T.Text -> Maybe (Transition Integer)
parseTransition t =
  if t == "_"
    then pure Halt
    else do
      guard (T.length t >= 2)
      let (action, state) = T.splitAt 1 t
      Transition <$> parseAction action <*> parseStateName state

parseAction :: T.Text -> Maybe Action
parseAction "L" = Just $ MoveTo L
parseAction "R" = Just $ MoveTo R
parseAction "0" = Just $ SetTo B0
parseAction "1" = Just $ SetTo B1
parseAction _ = Nothing

parseBit :: Char -> Maybe Bit
parseBit '0' = Just B0
parseBit '1' = Just B1
parseBit _ = Nothing

parseTape :: T.Text -> Maybe Tape
parseTape = fmap (IntMap.fromAscList . L.zip [0 ..]) . traverse parseBit . T.unpack
