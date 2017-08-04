module Glucose.Error (CompileError, liftErrors, formatError) where

import Control.Comonad
import Control.Monad.Except
import Data.Semigroup
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Glucose.Identifier (Identifier)
import Glucose.IR.Checked (Type)
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

instance (Located (f Identifier), Located (f Type)) => Located (TypeChecker.TypeCheckError f) where
  location (TypeChecker.DuplicateDefinition a _) = location a
  location (TypeChecker.UnrecognisedVariable a) = location a
  location (TypeChecker.RecursiveDefinition a) = location a
  location (TypeChecker.TypeMismatch a _) = location a


-- * Error formatting

formatError :: Text -> CompileError -> Text
formatError source = \case
  SyntaxError (Lexer.SyntaxError loc details) -> withLocation loc $ case details of
    Lexer.SyntaxErrorDetails message context ->
      message <> " " <> context
  ParseError (Parser.ParseError loc details) -> withLocation loc $ case details of
    Parser.ParseErrorDetails unexpected expected ->
      "unexpected " <> fromUnexpected unexpected <>
      ("\nexpecting " <>) `ifNotNull` formatList (map formatEOF expected)
    where fromUnexpected = formatEOF . (showToken source <$>)
  TypeCheckError details -> withLocation (location details) $ case details of
    TypeChecker.DuplicateDefinition name prevLoc ->
      "duplicate definition of '" <> format name <> "'\n" <>
      "previously defined at " <> showLocation (startLocation prevLoc)
    TypeChecker.UnrecognisedVariable name ->
      "unrecognised variable '" <> format name <> "'"
    TypeChecker.RecursiveDefinition name ->
      "recursive definition: the value of '" <> format name <> "' depends on itself"
    TypeChecker.TypeMismatch a b ->
      "type mismatch: expected '" <> format a <> "', found '" <> format b <> "'" -- TODO: improve

withLocation :: Location -> Text -> Text
withLocation loc s = showLocation loc <> ":\n" <> s <> "\n"

format :: (Show a, Comonad f) => f a -> Text
format = pack . show . extract

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
