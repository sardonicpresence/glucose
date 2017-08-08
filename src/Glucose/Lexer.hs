{-# LANGUAGE TemplateHaskell #-}
module Glucose.Lexer (tokens, tokenise) where

import Control.Comonad
import Control.Lens
import Control.Lens.TH ()
import Control.Monad.Except
import Control.Monad.RWS
import Data.Char
import Data.Foldable
import Data.Maybe
import Data.Text (Text, pack, unpack)

import Glucose.Lexer.Char
import Glucose.Lexer.NumericLiteral
import Glucose.Lexer.SyntaxError
import Glucose.Source
import Glucose.Token

type Tokenised = (Location, [FromSource Token])

data PartialLexeme
  = StartOfLine
  | Indentation
  | Gap
  | Token Token
  | PartialIdentifier String -- reversed
  | PartialOperator String -- reversed
  | NumericLiteral NumericLiteral
  | EOF

data Lexer = Lexer { _partial :: PartialLexeme, _lexemeStart :: Location, _pos :: Location, _inDefinition :: Bool }

makeLenses ''Lexer

initLexer :: Lexer
initLexer = Lexer StartOfLine beginning beginning False

-- | Perform lexical analysis, splitting UTF8 text into tokens.
tokens :: MonadError SyntaxError m => Text -> m [Token]
tokens input = map extract . extract <$> tokenise input

-- | Perform lexical analysis, splitting UTF8 text into tokens.
-- Evaluates to a list of tokens, each associated with a range of characters, paired with the location of EOF.
tokenise :: MonadError SyntaxError m => Text -> m Tokenised
tokenise = runLexer . traverse_ consume . unpack

type Lex m a = RWST () [FromSource Token] Lexer m a

runLexer :: MonadError SyntaxError m => Lex m () -> m Tokenised
runLexer l = (_1 %~ view pos) <$> execRWST (l *> completeLexeme Nothing) () initLexer

consume :: MonadError SyntaxError m => Char -> Lex m ()
consume c = consumeChar c *> (pos %= updateLocation c)

consumeChar :: MonadError SyntaxError m => Char -> Lex m ()
consumeChar c = maybe (completeLexeme $ Just c) (partial .=) =<< maybeAppend c =<< use partial

maybeAppend :: MonadError SyntaxError m => Char -> PartialLexeme -> Lex m (Maybe PartialLexeme)
maybeAppend c StartOfLine | isNewline c = pure $ Just StartOfLine
maybeAppend c StartOfLine | isSpace c = pure $ Just Indentation
maybeAppend c Indentation | isNewline c = pure $ Just StartOfLine
maybeAppend c Indentation | isSpace c = pure $ Just Indentation
maybeAppend c Gap | isNewline c = pure $ Just StartOfLine
maybeAppend c Gap | isSpace c = pure $ Just Gap
maybeAppend c (PartialIdentifier cs) | isIdentifier c = pure . Just $ PartialIdentifier (c:cs)
maybeAppend c (PartialOperator cs) | isOperator c = pure . Just $ PartialOperator (c:cs)
maybeAppend c (NumericLiteral lit) = do
  lit' <- locateError $ extendNumericLiteral c lit
  maybe nextLexemeOrError (pure . Just .  NumericLiteral) lit'
  where nextLexemeOrError = Nothing <$ when (isIdentifier c) (unexpectedChar c "in numeric literal")
maybeAppend _ _ = pure Nothing

startingWith :: MonadError SyntaxError m => Char -> Lex m PartialLexeme
startingWith '\\' = pure $ Token BeginLambda
startingWith '(' = pure $ Token OpenParen
startingWith ')' = pure $ Token CloseParen
startingWith c | isNewline c = pure StartOfLine
startingWith c | isSpace c || isControl c = pure Gap
startingWith c | isDigit c = pure $ NumericLiteral $ numericLiteral (digitToInt c)
startingWith c | isIdentifier c = pure $ PartialIdentifier [c]
startingWith c | isOperator c = pure $ PartialOperator [c]
startingWith c = unexpectedChar c "in input"

completeLexeme :: MonadError SyntaxError m => Maybe Char -> Lex m ()
completeLexeme nextChar = use partial >>= \case
  EOF -> error "Still lexing after eof!"
  StartOfLine -> unless (isNothing nextChar) $ implicitEndOfDefinition *> nextLexeme nextChar
  Indentation -> do
    indentedDefinition <- uses inDefinition $ (isJust nextChar &&) . not
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
    (token, lastChar) <- locateError $ completeNumericLiteral lit
    case lastChar of
      Nothing -> tellLexeme nextChar token
      Just lc -> do
        pos %= rewind
        case nextChar of
          Nothing -> unexpectedChar lc "following numeric literal"
          Just nc -> do
            tellLexeme lastChar token
            pos %= updateLocation lc
            consumeChar nc

locateError :: MonadError SyntaxError m => Lex (Either SyntaxErrorDetails) a -> Lex m a
locateError m = use pos >>= \loc -> mapRWST (either (throwError . SyntaxError loc) pure) m

implicitEndOfDefinition :: Monad m => Lex m ()
implicitEndOfDefinition = do
  start <- use lexemeStart
  when (start /= beginning) $ tellToken start start EndOfDefinition

tellLexeme :: MonadError SyntaxError m => Maybe Char -> Token -> Lex m ()
tellLexeme nextChar token = do
  start <- use lexemeStart
  pos <- use pos
  tellToken start pos token
  nextLexeme nextChar

tellToken :: Monad m => Location -> Location -> Token -> Lex m ()
tellToken start after token = tell [FromSource (SourceRange start $ rewind after) token]

nextLexeme :: MonadError SyntaxError m => Maybe Char -> Lex m ()
nextLexeme nextChar =  do
  partial <~ maybe (pure EOF) startingWith nextChar
  lexemeStart <~ use pos
  inDefinition .= True


-- * Error messages

syntaxError :: MonadError SyntaxError m => Text -> Text -> Lex m a
syntaxError message context = use pos >>= \loc -> throwError . SyntaxError loc $ SyntaxErrorDetails message context

unexpected :: MonadError SyntaxError m => String -> Text -> Lex m a
unexpected u = syntaxError ("unexpected " <> pack u)

unexpectedChar :: MonadError SyntaxError m => Char -> Text -> Lex m a
unexpectedChar c = unexpected (show c)
