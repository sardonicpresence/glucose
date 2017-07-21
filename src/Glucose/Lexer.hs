module Glucose.Lexer (tokens, tokenise, tokeniseReversible) where

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
import Glucose.Lexer.Location
import Glucose.Lexer.NumericLiteral
import Glucose.Lexer.SyntacticToken
import Glucose.Parser.Source
import Glucose.Token

-- | The output of the lexer with the location of the end of the input.
type Tokenised a = (Location, a)

tokens :: Text -> Either CompileError [Token]
tokens input = map extract . extract <$> tokenise input

tokenise :: Error m => Text -> m (Tokenised [FromSource Token])
tokenise input = fmap (mapMaybe fromLexeme) <$> lexemes input

tokeniseReversible :: Text -> Either CompileError (Tokenised [SyntacticToken])
tokeniseReversible input = fmap fromLexemes <$> lexemes input where
  fromLexemes as = mapMaybe (uncurry $ syntacticToken input) $ zip (Lexeme Nothing beginning 0 : as) as

lexemes :: Error m => Text -> m (Tokenised [Lexeme])
lexemes = runLexer . traverse_ consume . unpack

data PartialLexeme
  = StartOfLine
  | Indentation
  | Gap
  | Token Token
  | PartialIdentifier String -- reversed
  | PartialOperator String -- reversed
  | NumericLiteral NumericLiteral

data Lexer = Lexer { partial :: PartialLexeme, lexemeStart :: Location, pos :: Location, inDefinition :: Bool }

initLexer :: Lexer
initLexer = Lexer StartOfLine beginning beginning False

_pos :: Lens Lexer Lexer Location Location
_pos = lens pos (\lexer pos' -> lexer {pos = pos'})

_partial :: Lens Lexer Lexer PartialLexeme PartialLexeme
_partial = lens partial (\lexer partial' -> lexer {partial = partial'})

type Lex m a = RWST () [Lexeme] Lexer m a

runLexer :: Error m => Lex m () -> m (Tokenised [Lexeme])
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
  StartOfLine -> unless (isNothing nextChar) $ implicitEndOfDefinition *> tellLexeme nextChar Nothing
  Indentation -> do
    indentedDefinition <- ((isJust nextChar &&) . not) <$> gets inDefinition
    when indentedDefinition $ unexpected "indentation" "before first definition"
    tellLexeme nextChar Nothing
  Gap -> tellLexeme nextChar Nothing
  Token token -> tellLexeme nextChar $ Just token
  PartialIdentifier "epyt" -> tellLexeme nextChar $ Just $ Keyword Type
  PartialIdentifier s -> tellLexeme nextChar $ Just $ Identifier $ pack $ reverse s
  PartialOperator "=" -> tellLexeme nextChar $ Just $ Operator Assign
  PartialOperator ":" -> tellLexeme nextChar $ Just $ Operator Colon
  PartialOperator "|" -> tellLexeme nextChar $ Just $ Operator Bar
  PartialOperator ">-" -> tellLexeme nextChar $ Just $ Operator Arrow
  PartialOperator cs -> tellLexeme nextChar $ Just $ Operator $ CustomOperator (pack $ reverse cs)
  NumericLiteral lit -> do
    (token, lastChar) <- toLex $ completeNumericLiteral lit
    case lastChar of
      Nothing -> tellLexeme nextChar (Just token)
      Just lc -> do
        _pos %= rewind
        case nextChar of
          Nothing -> unexpectedChar lc "following numeric literal"
          Just nc -> do
            tellLexeme lastChar (Just token)
            _pos %= updateLocation lc
            consumeChar nc

toLex :: Error m => RWST () [Lexeme] Lexer (Either ErrorDetails) a -> Lex m a
toLex m = gets pos >>= \loc -> mapRWST (locateError loc) m

implicitEndOfDefinition :: Monad m => Lex m ()
implicitEndOfDefinition = do
  start <- gets lexemeStart
  when (start /= beginning) $ tell $ pure $ Lexeme (Just EndOfDefinition) start 0

tellLexeme :: Error m => Maybe Char -> Maybe Token -> Lex m ()
tellLexeme nextChar token = do
  Lexer { lexemeStart, pos } <- get
  tell . pure $ Lexeme token lexemeStart (codePointsBetween lexemeStart pos)
  partial' <- maybe (pure undefined) startingWith nextChar
  put $ Lexer partial' pos pos True


-- * Error messages

unexpected :: Error m => String -> String -> Lex m a
unexpected u s = gets pos >>= \loc -> Error.unexpected loc u s

unexpectedChar :: Error m => Char -> String -> Lex m a
unexpectedChar c = unexpected (show c)
