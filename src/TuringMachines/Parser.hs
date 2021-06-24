module TuringMachines.Parser where

import qualified Data.IntMap.Strict as IntMap
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import TuringMachines.Core
import Utils.Parsing
import Prelude hiding (lines)

type Parser = Parsec Void T.Text

pProgram :: Parser (Program Integer)
pProgram = do
  spaceConsumer
  items <- sepByReqNewLine (pStateSpec <* horizontalSpace)
  spaceConsumer
  eof
  pure $ M.fromList items

pStateSpec :: Parser (Integer, Spec Integer)
pStateSpec = (,) <$> (pQstate <* hspace1) <*> pSpec

pSpec :: Parser (Spec Integer)
pSpec = Spec <$> (pTransition <* hspace1) <*> pTransition

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
