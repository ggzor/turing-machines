module TuringMachines.Parser where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import TuringMachines.Core
import Prelude hiding (lines)

type Parser = Parsec Void T.Text

pProgram :: Parser (Program Integer)
pProgram = do
  separatorSpace
  items <- pManyStateSpec
  separatorSpace
  eof
  pure $ M.fromList items

pManyStateSpec :: Parser [(Integer, Spec Integer)]
pManyStateSpec = do
  mSpec <- optional (pStateSpec <* horizontalSpace)
  case mSpec of
    Nothing -> pure []
    Just s1 -> do
      -- This check is needed to make sure a newline is between specs
      nl <- optional newline
      case nl of
        Nothing -> pure [s1]
        Just _ -> do
          separatorSpace
          (s1 :) <$> pManyStateSpec

horizontalSpace :: Parser ()
horizontalSpace = L.space hspace1 (L.skipLineComment "--") empty

separatorSpace :: Parser ()
separatorSpace =
  L.space
    space1
    (L.skipLineComment "--")
    empty

pStateSpec :: Parser (Integer, Spec Integer)
pStateSpec = (,) <$> (pQstate <* hspace1) <*> pSpec

pSpec :: Parser (Spec Integer)
pSpec = do
  t1 <- pTransition
  hspace1
  t2 <- pTransition
  pure $ Spec t1 t2

pTransition :: Parser (Transition Integer)
pTransition = (Transition <$> pAction <*> pQstate) <|> Halt <$ char '_'

pQstate :: Parser Integer
pQstate = char 'q' *> L.decimal

pAction :: Parser Action
pAction =
  choice
    [ SetTo B0 <$ char '0'
    , SetTo B1 <$ char '1'
    , MoveTo L <$ char 'L'
    , MoveTo R <$ char 'R'
    ]

pTape :: Parser Tape
pTape = IntMap.fromAscList . zip [0 ..] <$> many pBit

pBit :: Parser Bit
pBit =
  choice
    [ B0 <$ char '0'
    , B1 <$ char '1'
    ]
