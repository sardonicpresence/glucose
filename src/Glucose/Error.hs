module Glucose.Error
(
  module Control.Monad.Except,
  CompileError(..), ErrorDetails(..), Error,
  formatError,
  syntaxError, unexpected, duplicateDefinition, unrecognisedVariable, recursiveDefinition, typeMismatch,
  locateError
) where

import Control.Comonad
import Control.Monad.Except
import Data.Semigroup
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Glucose.Identifier
import Glucose.Parser.EOFOr
import Glucose.Source
import Glucose.Token as Token

data CompileError = CompileError { location :: Location, details :: ErrorDetails } deriving (Eq, Show)

instance Semigroup CompileError where
  a <> b | location a > location b = a
  a <> b | location b > location a = b
  a <> b = CompileError (location a) (details a <> details b)

data ErrorDetails
  = SyntaxError Text Text
  | Unexpected (Either Text (EOFOr (FromSource Token))) (Maybe Text) [Text]
  | DuplicateDefinition Identifier Location
  | UnrecognisedVariable Identifier
  | RecursiveDefinition Identifier
  | TypeMismatch Text Text
  deriving (Eq, Show)

instance Semigroup ErrorDetails where
  (Unexpected thing context expectations1) <> (Unexpected _ _ expectations2) =
    Unexpected thing context (expectations1 <> expectations2)
  a <> _ = a

formatError :: Text -> CompileError -> Text
formatError source (CompileError loc details) = showLocation loc <> ":\n" <> formatDetails source details <> "\n"

formatDetails :: Text -> ErrorDetails -> Text
formatDetails source = \case
  SyntaxError error context ->
    error <> " in " <> context
  Unexpected thing context expectations ->
    "unexpected " <> fromUnexpected thing source <> maybe mempty (" " <>) context <>
    ("\nexpecting " <>) `ifNotNull` formatList expectations
    where fromUnexpected = either const $ maybeEOF (const "end of file") (flip showToken)
  DuplicateDefinition name prevLoc ->
    "duplicate definition of '" <> pack (show name) <> "'\n" <>
    "previously defined at " <> showLocation prevLoc
  UnrecognisedVariable name ->
    "unrecognised variable '" <> pack (show name) <> "'"
  RecursiveDefinition name ->
    "recursive definition: the value of '" <> pack (show name) <> "' depends on itself"
  TypeMismatch a b ->
    "type mismatch: expected '" <> a <> "', found '" <> b <> "'" -- TODO: improve

formatList :: [Text] -> Text
formatList [] = mempty
formatList [a] = a
formatList [a, b] = a <> " or " <> b
formatList (a:as) = a <> ", " <> formatList as

ifNotNull :: (Text -> Text) -> Text -> Text
ifNotNull f a = if Text.null a then a else f a

type Error m = MonadError CompileError m

syntaxError :: Error m => Location -> String -> String -> m a
syntaxError loc a b = throwError $ CompileError loc $ SyntaxError (pack a) (pack b)

unexpected :: Error m => Location -> String -> String -> m a
unexpected loc a b = throwError $ CompileError loc $ Unexpected (Left $ pack a) (Just $ pack b) []

duplicateDefinition :: Error m => Location -> Identifier -> Location -> m a
duplicateDefinition loc a b = throwError $ CompileError loc $ DuplicateDefinition a b

unrecognisedVariable :: Error m => Location -> Identifier -> m a
unrecognisedVariable loc a = throwError $ CompileError loc $ UnrecognisedVariable a

recursiveDefinition :: Error m => Location -> Identifier -> m a
recursiveDefinition loc a = throwError $ CompileError loc $ RecursiveDefinition a

typeMismatch :: (Error m, Show t) => Location -> t -> t -> m a
typeMismatch loc a b = throwError $ CompileError loc $ TypeMismatch (pack $ show a) (pack $ show b)

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

locateError :: Error m => Location -> Either ErrorDetails a -> m a
locateError loc (Left e) = throwError $ CompileError loc e
locateError _ (Right a) = pure a
