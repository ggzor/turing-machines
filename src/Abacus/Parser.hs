module Abacus.Parser where

import Abacus.Core

import Data.Text (Text)
import qualified Data.Text as T
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Utils.Parsing

type Parser = Parsec Void Text

-- MacroCall is the only dirty trick here,
-- it is used with GoTo to represent a macro expansion
data MacroTag = MacroBegin | MacroEnd | NamedTag Text | MacroCall Text [Text] deriving (Eq, Show)
data CellRef = CellNumber Integer | CellNamed Text deriving (Eq, Show)

data Macro = Macro Text [Text] (FlowChart MacroTag CellRef) deriving (Eq, Show)
type Program = [Macro]

pProgram :: Parser Program
pProgram = do
  anySpace
  macros <- (anySpace *> pMacro) `trySepBy` verticalSpace
  anySpace
  eof
  pure macros

pMacro :: Parser Macro
pMacro = do
  name <- pIdentifier
  params <- hbetweenParens (pIdentifier `sepBy` hsymbol ",")
  _ <- hsymbol ":"
  Macro name params <$> forceNL (pure []) pFlowChart

pFlowChart :: Parser (FlowChart MacroTag CellRef)
pFlowChart = do
  untaggedStartSeq <- pSeq MacroBegin <* horizontalSpace
  remainingTaggedSeqs <- forceNL (pure []) $ pTaggedSeq `trySepBy` verticalSpace
  let allSeqs = untaggedStartSeq : remainingTaggedSeqs
      mergeSeqs genSeq (seqs, curTag) =
        let nextSeq@(Seq newTag instructions _) = genSeq curTag
         in if newTag == MacroBegin && null instructions
              then (seqs, curTag)
              else (nextSeq : seqs, newTag)
  pure . fst $ foldr mergeSeqs ([], MacroEnd) allSeqs

pTaggedSeq :: Parser (MacroTag -> Seq MacroTag CellRef)
pTaggedSeq = do
  tagName <- try (anySpace *> pIdentifierName <* hsymbol ":")
  forceNL (fail "missing newline after colon") $ pSeq (NamedTag tagName)

pSeq :: MacroTag -> Parser (MacroTag -> Seq MacroTag CellRef)
pSeq begin = do
  inc <- try (anySpace *> pNode) `trySepBy` verticalSpace
  pure $ \end -> Seq begin inc end

pNode :: Parser (Node MacroTag CellRef)
pNode =
  choice
    [ try $ Increase <$> pCellRef <* hsymbol "+"
    , try $
        Decrease
          <$> (pCellRef <* hsymbol "-" <* horizontalSpace)
          <*> (NamedTag <$> pIdentifier)
    , try $ do
        name <- pIdentifier
        args <- hbetweenParens (pIdentifier `sepBy` hsymbol ",")
        notFollowedBy $ char ':'
        pure $ GoTo (MacroCall name args)
    , GoTo . NamedTag <$> pIdentifier <* notFollowedBy (choice [char ':', char '('])
    ]

pCellRef :: Parser CellRef
pCellRef =
  choice
    [ CellNumber <$> L.decimal
    , CellNamed <$> pIdentifierName
    ]

pIdentifier :: Parser Text
pIdentifier = lexeme pIdentifierName

pIdentifierName :: Parser Text
pIdentifierName = T.pack <$> ((:) <$> letterChar <*> many (alphaNumChar <|> char '_'))

lexeme :: Parser a -> Parser a
lexeme = L.lexeme horizontalSpace

hsymbol :: Text -> Parser Text
hsymbol = L.symbol horizontalSpace

hbetweenParens :: Parser a -> Parser a
hbetweenParens = between (hsymbol "(") (hsymbol ")")

forceNL :: Parser a -> Parser a -> Parser a
forceNL f p = do
  hasNL <- followedBy newline
  if hasNL
    then p
    else f
