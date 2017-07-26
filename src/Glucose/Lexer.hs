module Glucose.Lexer (tokens, tokenise) where

import Control.Comonad
import Control.Lens
import Control.Monad.RWS
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Text (Text, pack, unpack)

import Glucose.Error hiding (unexpected)
import qualified Glucose.Error as Error
import Glucose.Lexer.Char
import Glucose.Lexer.NumericLiteral
import Glucose.Source
import Glucose.Token

type Tokenised = (Location, [FromSource Token])

tokens :: Text -> Either CompileError [Token]
tokens input = map extract . extract <$> tokenise input

-- | Perform lexical analysis, splitting UTF8 text into tokens.
-- Evaluates to a list of tokens, each associated with a range of characters, paired with the location of EOF.
tokenise :: Error m => Text -> m Tokenised
tokenise = runLexer . traverse_ consume . unpack

data PartialLexeme
  = StartOfLine
  | Indentation
  | Gap
  | Token Token
  | PartialIdentifier String -- reversed
  | PartialOperator String -- reversed
  | NumericLiteral NumericLiteral
  | EOF

data Lexer = Lexer { partial :: PartialLexeme, lexemeStart :: Location, pos :: Location, inDefinition :: Bool }

initLexer :: Lexer
initLexer = Lexer StartOfLine beginning beginning False

_pos :: Lens Lexer Lexer Location Location
_pos = lens pos (\lexer pos' -> lexer {pos = pos'})

_partial :: Lens Lexer Lexer PartialLexeme PartialLexeme
_partial = lens partial (\lexer partial' -> lexer { partial = partial' })

type Lex m a = RWST () [FromSource Token] Lexer m a

runLexer :: Error m => Lex m () -> m Tokenised
runLexer l = (_1 %~ pos) <$> execRWST (l *> completeLexeme Nothing) () initLexer

consume :: Error m => Char -> Lex m ()
consume c = consumeChar c *> (_pos %= updateLocation c)

consumeChar :: Error m => Char -> Lex m ()
consumeChar c = maybe (completeLexeme $ Just c) (_partial .=) =<< maybeAppend c =<< gets partial

maybeAppend :: Error m => Char -> PartialLexeme -> Lex m (Maybe PartialLexeme)
maybeAppend c StartOfLine | isNewline c = pure $ Just StartOfLine
maybeAppend c StartOfLine | isSpace c = pure $ Just Indentation
maybeAppend c Indentation | isNewline c = pure $ Just StartOfLine
maybeAppend c Indentation | isSpace c = pure $ Just Indentation
maybeAppend c Gap | isNewline c = pure $ Just StartOfLine
maybeAppend c Gap | isSpace c = pure $ Just Gap
maybeAppend c (PartialIdentifier cs) | isIdentifier c = pure . Just $ PartialIdentifier (c:cs)
maybeAppend c (PartialOperator cs) | isOperator c = pure . Just $ PartialOperator (c:cs)
maybeAppend c (NumericLiteral lit) = do
  lit' <- toLex $ extendNumericLiteral c lit
  maybe nextLexemeOrError (pure . Just .  NumericLiteral) lit'
  where nextLexemeOrError = Nothing <$ when (isIdentifier c) (unexpectedChar c "in numeric literal")
maybeAppend _ _ = pure Nothing

startingWith :: Error m => Char -> Lex m PartialLexeme
startingWith '\\' = pure $ Token BeginLambda
startingWith '(' = pure $ Token OpenParen
startingWith ')' = pure $ Token CloseParen
startingWith c | isNewline c = pure StartOfLine
startingWith c | isSpace c || isControl c = pure Gap
startingWith c | isDigit c = pure $ NumericLiteral $ numericLiteral (digitToInt c)
startingWith c | isIdentifier c = pure $ PartialIdentifier [c]
startingWith c | isOperator c = pure $ PartialOperator [c]
startingWith c = unexpectedChar c "in input"

completeLexeme :: Error m => Maybe Char -> Lex m ()
completeLexeme nextChar = gets partial >>= \case
  EOF -> error "Still lexing after eof!"
  StartOfLine -> unless (isNothing nextChar) $ implicitEndOfDefinition *> nextLexeme nextChar
  Indentation -> do
    indentedDefinition <- ((isJust nextChar &&) . not) <$> gets inDefinition
    when indentedDefinition $ unexpected "indentation" "before first definition"
    nextLexeme nextChar
  Gap -> nextLexeme nextChar
  Token token -> tellLexeme nextChar token
  PartialIdentifier "epyt" -> tellLexeme nextChar $ Keyword Type
  PartialIdentifier s -> tellLexeme nextChar $ Identifier $ pack $ reverse s
  PartialOperator "=" -> tellLexeme nextChar $ Operator Assign
  PartialOperator ":" -> tellLexeme nextChar $ Operator Colon
  PartialOperator "|" -> tellLexeme nextChar $ Operator Bar
  PartialOperator ">-" -> tellLexeme nextChar $ Operator Arrow
  PartialOperator cs -> tellLexeme nextChar $ Operator $ CustomOperator (pack $ reverse cs)
  NumericLiteral lit -> do
    (token, lastChar) <- toLex $ completeNumericLiteral lit
    case lastChar of
      Nothing -> tellLexeme nextChar token
      Just lc -> do
        _pos %= rewind
        case nextChar of
          Nothing -> unexpectedChar lc "following numeric literal"
          Just nc -> do
            tellLexeme lastChar token
            _pos %= updateLocation lc
            consumeChar nc

toLex :: Error m => RWST () [FromSource Token] Lexer (Either ErrorDetails) a -> Lex m a
toLex m = gets pos >>= \loc -> mapRWST (locateError loc) m

implicitEndOfDefinition :: Monad m => Lex m ()
implicitEndOfDefinition = do
  start <- gets lexemeStart
  when (start /= beginning) $ tellToken start start EndOfDefinition

tellLexeme :: Error m => Maybe Char -> Token -> Lex m ()
tellLexeme nextChar token = do
  Lexer { lexemeStart, pos } <- get
  tellToken lexemeStart pos token
  nextLexeme nextChar

tellToken :: Monad m => Location -> Location -> Token -> Lex m ()
tellToken start after token = tell [FromSource (SourceRange start $ rewind after) token]

nextLexeme :: Error m => Maybe Char -> Lex m ()
nextLexeme nextChar =  do
  partial' <- maybe (pure EOF) startingWith nextChar
  modify $ \lexer -> lexer { partial = partial', lexemeStart = pos lexer, inDefinition = True }


-- * Error messages

unexpected :: Error m => String -> String -> Lex m a
unexpected u s = gets pos >>= \loc -> Error.unexpected loc u s

unexpectedChar :: Error m => Char -> String -> Lex m a
unexpectedChar c = unexpected (show c)
