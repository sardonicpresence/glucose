module Glucose.Lexer (tokens, tokenize, tokenize_) where

import Control.Lens
import Control.Monad.Except
import Control.Monad.RWS
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Text (Text, pack, unpack)

import Glucose.Error
import Glucose.Lexer.Char
import Glucose.Lexer.Location
import Glucose.Lexer.NumericLiteral
import Glucose.Lexer.Lexeme
import Glucose.Token

data PartialLexeme
  = StartOfLine
  | Indentation
  | Gap
  | PartialIdentifier String -- reversed
  | PartialOperator String -- reversed
  | NumericLiteral NumericLiteral

data Lexer = Lexer { partial :: PartialLexeme, lexemeStart :: Location, pos :: Location }

_pos :: Lens Lexer Lexer Location Location
_pos = lens pos (\lexer pos' -> lexer {pos = pos'})

_partial :: Lens Lexer Lexer PartialLexeme PartialLexeme
_partial = lens partial (\lexer partial' -> lexer {partial = partial'})

type Lex = RWST () [Lexeme] Lexer Error

tokens :: Text -> Error [Token]
tokens = fmap (map token) . tokenize

tokenize :: Text -> Error [SyntacticToken]
tokenize input = mapMaybe (syntacticToken input) <$> tokenize_ input

tokenize_ :: Text -> Error [Lexeme]
tokenize_ = runLexer . consume . unpack

runLexer :: Lex () -> Error [Lexeme]
runLexer l = snd <$> execRWST (l *> completeLexeme Nothing) () (Lexer StartOfLine beginning beginning)

consume :: String -> Lex ()
consume = traverse_ $ \c -> do
  maybe (completeLexeme $ Just c) (_partial .=) =<< maybeAppend c =<< gets partial
  _pos %= updateLocation c

maybeAppend :: MonadError SyntaxError m => Char -> PartialLexeme -> m (Maybe PartialLexeme)
maybeAppend c StartOfLine | isNewline c = pure $ Just StartOfLine
maybeAppend c StartOfLine | isSpace c = pure $ Just Indentation
maybeAppend c Indentation | isNewline c = pure $ Just StartOfLine
maybeAppend c Indentation | isSpace c = pure $ Just Indentation
maybeAppend c Gap | isNewline c = pure $ Just StartOfLine
maybeAppend c Gap | isSpace c = pure $ Just Gap
maybeAppend c (PartialIdentifier cs) | isIdentifier c = pure . Just $ PartialIdentifier (c:cs)
maybeAppend c (PartialOperator cs) | isOperator c = pure . Just $ PartialOperator (c:cs)
maybeAppend c (NumericLiteral lit) = maybe nextLexemeOrError (pure . Just .  NumericLiteral) $ extendNumericLiteral c lit
  where nextLexemeOrError = if isIdentifier c then unexpected c "in numeric literal" else pure Nothing
maybeAppend _ _ = pure Nothing

startingWith :: MonadError SyntaxError m => Char -> m PartialLexeme
startingWith c | isNewline c = pure StartOfLine
startingWith c | isSpace c || isControl c = pure Gap
startingWith c | isNumber c = pure $ NumericLiteral $ numericLiteral (digitToInt c)
startingWith c | isIdentifier c = pure $ PartialIdentifier [c]
startingWith c | isOperator c = pure $ PartialOperator [c]
startingWith c = unexpected c "in input"

completeLexeme :: Maybe Char -> Lex ()
completeLexeme nextChar = gets partial >>= \case
  StartOfLine -> implicitEndOfDefinition *> tellLexeme nextChar Nothing
  Indentation -> tellLexeme nextChar Nothing
  Gap -> tellLexeme nextChar Nothing
  PartialIdentifier s -> tellLexeme nextChar $ Just $ Identifier $ pack $ reverse s
  PartialOperator "=" -> tellLexeme nextChar $ Just $ Operator Assign
  PartialOperator cs -> tellLexeme nextChar $ Just $ Operator $ CustomOperator (pack $ reverse cs)
  NumericLiteral lit -> do
    (token, lastChar) <- completeNumericLiteral lit
    -- TODO: The following code may contain bugs but is essentially unused
    case lastChar of
      Nothing -> tellLexeme nextChar (Just token)
      Just lc -> case nextChar of
        Nothing -> unexpected lc "following numeric literal"
        Just nc -> do
          Lexer { lexemeStart, pos } <- get
          tell . pure $ Lexeme (Just token) lexemeStart (codePointsBetween lexemeStart pos - 1)
          partial' <- startingWith lc
          put $ Lexer partial' pos pos
          maybe (completeLexeme $ Just nc) (_partial .=) =<< maybeAppend nc partial'

implicitEndOfDefinition :: (MonadState Lexer m, MonadWriter [Lexeme] m) => m ()
implicitEndOfDefinition = do
  start <- gets lexemeStart
  when (start /= beginning) $ tell $ pure $ Lexeme (Just EndOfDefinition) start 0

tellLexeme :: (MonadWriter [Lexeme] m, MonadState Lexer m, MonadError SyntaxError m) => Maybe Char -> Maybe Token -> m ()
tellLexeme nextChar token = do
  Lexer { lexemeStart, pos } <- get
  tell . pure $ Lexeme token lexemeStart (codePointsBetween lexemeStart pos)
  partial' <- maybe (pure undefined) startingWith nextChar
  put $ Lexer partial' pos pos


-- * Error messages

unexpected :: MonadError SyntaxError m => Char -> String -> m a
unexpected c s = throwError $ "Unexpected " ++ show c ++ " " ++ s
