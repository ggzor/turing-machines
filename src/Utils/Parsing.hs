module Utils.Parsing where

import Data.String (IsString)
import Text.Megaparsec
import Text.Megaparsec.Char (hspace1, newline, space1)
import qualified Text.Megaparsec.Char.Lexer as L

spaceConsumer :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m ()
spaceConsumer =
  L.space
    space1
    (L.skipLineComment "--")
    (L.skipBlockComment "{-" "-}")

horizontalSpace :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m ()
horizontalSpace = L.space hspace1 (L.skipLineComment "--") empty

sepByReqNewLine :: (MonadParsec e s m, Token s ~ Char, IsString (Tokens s)) => m a -> m [a]
sepByReqNewLine p = do
  mSpec <- optional p
  case mSpec of
    Nothing -> pure []
    Just s1 -> do
      -- This check is needed to make sure a newline is in between
      nl <- optional newline
      case nl of
        Nothing -> pure [s1]
        Just _ -> do
          spaceConsumer
          (s1 :) <$> sepByReqNewLine p
