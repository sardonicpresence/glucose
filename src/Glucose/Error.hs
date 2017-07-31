module Glucose.Error
-- (
--   module Control.Monad.Except,
--   Error, CompileError(..), ErrorDetails(..),
--   syntaxError, unexpected, duplicateDefinition, unrecognisedVariable, recursiveDefinition, typeMismatch,
--   formatError,
-- )
where

import Control.Comonad
import Control.Monad.Except
import Data.Semigroup
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Glucose.Lexer.SyntaxError
import Glucose.Parser.EOFOr
import Glucose.Parser.ParseError
import Glucose.Source
import Glucose.Token as Token
import Glucose.TypeChecker.TypeCheckError

data CompileError = forall e. ErrorDetails e => CompileError e

liftErrors :: (ErrorDetails e, MonadError CompileError m) => Either e a -> m a
liftErrors = either (throwError . CompileError) pure


-- * Error formatting

class ErrorDetails e where
  formatDetails :: Text -> e -> Located Text

instance ErrorDetails (Located SyntaxErrorDetails) where
  formatDetails _ = fmap $ \case
    SyntaxError error context -> error <> " " <> context

instance ErrorDetails (Located ParseErrorDetails) where
  formatDetails source = fmap $ \case
    ParseError unexpected expected ->
      "unexpected " <> fromUnexpected unexpected <>
      ("\nexpecting " <>) `ifNotNull` formatList (map formatEOF expected)
      where fromUnexpected = formatEOF . (showToken source <$>)

instance ErrorDetails (TypeCheckError FromSource) where
  formatDetails _ = \case
    DuplicateDefinition name prevLoc -> Located (startLocation name) $
      "duplicate definition of '" <> format name <> "'\n" <>
      "previously defined at " <> showLocation (startLocation prevLoc)
    UnrecognisedVariable name -> Located (startLocation name) $
      "unrecognised variable '" <> format name <> "'"
    RecursiveDefinition name -> Located (startLocation name) $
      "recursive definition: the value of '" <> format name <> "' depends on itself"
    TypeMismatch a b -> Located (startLocation a) $
      "type mismatch: expected '" <> format a <> "', found '" <> format b <> "'" -- TODO: improve

format :: (Show a, Comonad f) => f a -> Text
format = pack . show . extract

formatError :: Text -> CompileError -> Text
formatError source (CompileError details) = case formatDetails source details of
  Located loc description -> showLocation loc <> ":\n" <> description <> "\n"

formatEOF :: EOFOr Text -> Text
formatEOF = fromEOF "end of file"

formatList :: [Text] -> Text
formatList [] = mempty
formatList [a] = a
formatList [a, b] = a <> " or " <> b
formatList (a:as) = a <> ", " <> formatList as

ifNotNull :: (Text -> Text) -> Text -> Text
ifNotNull f a = if Text.null a then a else f a

showLocation :: Location -> Text
showLocation loc = pack $ show (line loc) ++ ":" ++ show (column loc)

showToken :: Text -> FromSource Token -> Text
showToken input t = let s = showSource t input in case extract t of
  EndOfDefinition -> (if Text.null s then "implicit" else "explicit") <> " end of definition"
  BeginLambda -> "lambda '" <> s <> "'"
  Token.Identifier _ -> "identifier '" <> s <> "'"
  Keyword _ -> "keyword '" <> s <> "'"
  Operator _ -> "operator '" <> s <> "'"
  OpenParen -> "("
  CloseParen -> ")"
  IntegerLiteral _ -> "integer literal '" <> s <> "'"
  FloatLiteral _ -> "fractional literal '" <> s <> "'"
