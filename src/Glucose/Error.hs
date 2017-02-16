module Glucose.Error
(
  module Control.Monad.Throw,
  CompileError(..), ErrorDetails(..), Error,
  formatError,
  syntaxError, unexpected, duplicateDefinition, unrecognisedVariable, recursiveDefinition, typeMismatch,
  locateError
) where

import Control.Monad.Throw
import Data.List
import Data.Monoid
import Data.Text (Text, pack)
import qualified Data.Text as Text
import Glucose.Identifier
import Glucose.Lexer.Location
import Glucose.Lexer.SyntacticToken (showToken)
import Glucose.Parser.EOFOr
import Glucose.Parser.Monad (ParseError(ParseError))
import Glucose.Parser.Source
import Glucose.Token

data CompileError = CompileError Location ErrorDetails deriving (Eq, Show)

data ErrorDetails
  = SyntaxError Text Text
  | Unexpected (Either Text (EOFOr (FromSource Token))) (Maybe Text) [Text]
  | DuplicateDefinition Identifier Location
  | UnrecognisedVariable Identifier
  | RecursiveDefinition Identifier
  | TypeMismatch Text Text
  deriving (Eq, Show)

formatError :: Text -> CompileError -> Text
formatError source (CompileError loc details) = showLocation loc <> ":\n" <> formatDetails source details

formatDetails :: Text -> ErrorDetails -> Text
formatDetails source = \case
  SyntaxError error context ->
    error <> " in " <> context
  Unexpected thing context expectations ->
    "unexpected " <> fromUnexpected thing source <> maybe mempty (" " <>) context <>
    ("\nexpecting " <>) `ifNotNull` formatList expectations
    where fromUnexpected = either const $ maybeEOF (const "end of file") (flip showToken)
  DuplicateDefinition name prevLoc ->
    "duplicate definition of ''" <> pack (show name) <> "'\n" <>
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

type Error m = MonadThrow CompileError m

instance MonadThrow CompileError m => MonadThrow (ParseError Location (FromSource Token)) m where
  throwError (ParseError location unexpected expected) = throwError $ CompileError location $
    Unexpected (Right unexpected) Nothing (fromExpected expected) where
    fromExpected = map (maybeEOF "end of file" pack) . nub

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

locateError :: MonadThrow CompileError m => Location -> Either ErrorDetails a -> m a
locateError loc (Left e) = throwError $ CompileError loc e
locateError _ (Right a) = pure a
