module Glucose.Error (CompileError(..), liftErrors, formatError) where

import Control.Comonad
import Control.Monad.Except
import Data.Semigroup
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text
import qualified Glucose.Format as Format
import qualified Glucose.Lexer.SyntaxError as Lexer
import Glucose.Parser.EOFOr
import qualified Glucose.Parser.ParseError as Parser
import Glucose.Source
import Glucose.Token as Token
import qualified Glucose.TypeChecker.TypeCheckError as TypeChecker

data CompileError
  = SyntaxError Lexer.SyntaxError
  | ParseError Parser.ParseError
  | TypeCheckError (TypeChecker.TypeCheckError FromSource)
  deriving (Eq)

instance Show CompileError where
  show = unpack . formatError ""

liftErrors :: (ErrorDetails e, MonadError CompileError m) => Either e a -> m a
liftErrors = either (throwError . toCompileError) pure

class ErrorDetails e where
  toCompileError :: e -> CompileError

instance ErrorDetails Lexer.SyntaxError where
  toCompileError = SyntaxError

instance ErrorDetails Parser.ParseError where
  toCompileError = ParseError

instance ErrorDetails (TypeChecker.TypeCheckError FromSource) where
  toCompileError = TypeCheckError


-- * Error formatting

formatError :: Text -> CompileError -> Text
formatError source = \case
  SyntaxError (Lexer.SyntaxError loc details) -> withLocation (showLocation loc) $ case details of
    Lexer.SyntaxErrorDetails message context ->
      message <> " " <> context
  ParseError (Parser.ParseError loc details) -> withLocation (showLocation loc) $ case details of
    Parser.ParseErrorDetails unexpected expected ->
      "unexpected " <> fromUnexpected unexpected <>
      ("\nexpecting " <>) `ifNotNull` formatList (map formatEOF expected)
    where fromUnexpected = formatEOF . (showToken source <$>)
  TypeCheckError details -> withLocation (showSourceRangeOf details) $ case details of
    TypeChecker.DuplicateDefinition name prevLoc ->
      "duplicate definition of '" <> fformat name <> "'\n" <>
      "previously defined at " <> showSourceRange prevLoc
    TypeChecker.UnrecognisedVariable name ->
      "unrecognised variable '" <> fformat name <> "'"
    TypeChecker.RecursiveDefinition name ->
      "recursive definition: the value of '" <> fformat name <> "' depends on itself"
    TypeChecker.TypeMismatch expected actual ->
      "type mismatch: " <>
      "expected '" <> format expected <> "', " <>
      "found '" <> fformat actual <> "' " <>
      "for expression '" <> showSource actual source <> "'"
    TypeChecker.InfiniteType expected actual ->
      "infinite type: " <> fformat actual <> " ~ " <> format expected
    TypeChecker.LocalLambda _ ->
      "lambdas can only be bound to globals"
    TypeChecker.CAF _ ->
      "unsupported constant applicative form"
  where
    showSourceRangeOf (TypeChecker.DuplicateDefinition a _) = showSourceRange a
    showSourceRangeOf (TypeChecker.UnrecognisedVariable a) = showSourceRange a
    showSourceRangeOf (TypeChecker.RecursiveDefinition a) = showSourceRange a
    showSourceRangeOf (TypeChecker.TypeMismatch _ a) = showSourceRange a
    showSourceRangeOf (TypeChecker.InfiniteType _ a) = showSourceRange a
    showSourceRangeOf (TypeChecker.LocalLambda a) = showSourceRange a
    showSourceRangeOf (TypeChecker.CAF a) = showSourceRange a

withLocation :: Text -> Text -> Text
withLocation loc s = loc <> ":\n" <> s <> "\n"

format :: Format.Formattable Format.Format a => a -> Text
format = Format.format Format.User

fformat :: (Format.FormattableFunctor Format.Format f, Format.Formattable Format.Format a) => f a -> Text
fformat = Format.fformat Format.User

formatEOF :: EOFOr Text -> Text
formatEOF = fromEOF "end of file"

formatList :: [Text] -> Text
formatList [] = mempty
formatList [a] = a
formatList [a, b] = a <> " or " <> b
formatList (a:as) = a <> ", " <> formatList as

ifNotNull :: (Text -> Text) -> Text -> Text
ifNotNull f a = if Text.null a then a else f a

showSourceRange :: FromSource a -> Text
showSourceRange a = showLocation (startLocation a)
  -- Just the start location for now
  -- <> "-" <> showLocation (endLocation a)

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
