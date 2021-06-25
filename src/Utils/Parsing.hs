module Utils.Parsing where

import Control.Monad (void)
import Data.String (IsString)
import Text.Megaparsec
import Text.Megaparsec.Char (hspace1, newline, space1)
import qualified Text.Megaparsec.Char.Lexer as L

lineComment :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m ()
lineComment = L.skipLineComment "--"

blockComment :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m ()
blockComment = L.skipBlockComment "{-" "-}"

-- Any kind of space and comments
anySpace :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m ()
anySpace = L.space space1 lineComment blockComment

-- Just horizontal space, no newlines allowed
horizontalSpace :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m ()
horizontalSpace = L.space hspace1 lineComment empty

-- Strict single vertical space, at least a new line should be found
verticalSpace :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m ()
verticalSpace =
  let noRawNewLines = L.space hspace1 lineComment blockComment
   in void $ noRawNewLines >> newline >> noRawNewLines

followedBy :: MonadParsec e s m => m a -> m Bool
followedBy = option True . (False <$) . notFollowedBy

trySepBy :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m a -> m sep -> m [a]
trySepBy p sep = trySepBy1 p sep <|> pure []

trySepBy1 :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m a -> m sep -> m [a]
trySepBy1 p sep = do
  v <- p
  followedBy (sep *> p) >>= \case
    False -> pure [v]
    True -> do
      _ <- sep
      (v :) <$> trySepBy p sep
