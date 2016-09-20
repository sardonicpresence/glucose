module Glucose.Lexer (tokens, tokenise, tokenize) where

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

data PartialLexeme
  = StartOfLine
  | Indentation
  | Gap
  | Lambda
  | PartialIdentifier String -- reversed
  | PartialOperator String -- reversed
  | NumericLiteral NumericLiteral

data Lexer = Lexer { partial :: PartialLexeme, lexemeStart :: Location, pos :: Location, inDefinition :: Bool }

_pos :: Lens Lexer Lexer Location Location
_pos = lens pos (\lexer pos' -> lexer {pos = pos'})

_partial :: Lens Lexer Lexer PartialLexeme PartialLexeme
_partial = lens partial (\lexer partial' -> lexer {partial = partial'})

type Lex m a = RWST () [Lexeme] Lexer m a

tokens :: Text -> Either CompileError [Token]
tokens = (map token . snd <$>) . tokenize

tokenise :: Error m => Text -> m (Location, [FromSource Token])
tokenise input = (_2 %~ mapMaybe fromLexeme) <$> _tokenize input

tokenize :: Text -> Either CompileError (Location, [SyntacticToken])
tokenize input = (_2 %~ go) <$> _tokenize input where
  go as = mapMaybe (uncurry $ syntacticToken input) $ zip (Lexeme Nothing beginning 0 : as) as

_tokenize :: Error m => Text -> m (Location, [Lexeme])
_tokenize = runLexer . traverse_ consume . unpack

runLexer :: Error m => Lex m () -> m (Location, [Lexeme])
runLexer l = (_1 %~ pos) <$> execRWST (l *> completeLexeme Nothing) () (Lexer StartOfLine beginning beginning False)

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
startingWith '\\' = pure Lambda
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
  Lambda -> tellLexeme nextChar $ Just BeginLambda
  PartialIdentifier s -> tellLexeme nextChar $ Just $ Identifier $ pack $ reverse s
  PartialOperator "=" -> tellLexeme nextChar $ Just $ Operator Assign
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
