module Glucose.Lexer.SyntacticToken where

import Control.Lens
import Data.List as List
import Data.Monoid
import Data.Text as Text
import Glucose.Lexer.Location
import Glucose.Parser.Source
import Glucose.Token

data Lexeme = Lexeme (Maybe Token) Location Int
  deriving (Show)

fromLexeme :: Lexeme -> Maybe (FromSource Token)
fromLexeme (Lexeme token loc len) = FromSource (SourceRange loc (advance loc (len-1))) <$> token

data SyntacticToken = SyntacticToken { token :: Token, location :: Location, lexeme :: Text, leadingSpace :: Text }
  deriving (Eq, Ord, Show)

showToken :: Text -> FromSource Token -> Text
showToken input t = let s = showSource t input in case t ^. _fromSource of
  EndOfDefinition -> (if Text.null s then "implicit" else "explicit") <> " end of definition"
  BeginLambda -> "lambda '" <> s <> "'"
  Identifier _ -> "identifier '" <> s <> "'"
  Keyword _ -> "keyword '" <> s <> "'"
  Operator _ -> "operator '" <> s <> "'"
  IntegerLiteral _ -> "integer literal '" <> s <> "'"
  FloatLiteral _ -> "fractional literal '" <> s <> "'"

syntacticToken :: Text -> Lexeme -> Lexeme -> Maybe SyntacticToken
syntacticToken _ _ (Lexeme Nothing _ _) = Nothing
syntacticToken input (Lexeme prev prevLoc _) (Lexeme (Just token) loc len) =
  Just $ SyntacticToken token loc lexeme leadingSpace where
    ws = maybe (codePointsBetween prevLoc loc) (const 0) prev
    (leadingSpace, lexeme) = Text.splitAt ws . Text.take (len + ws) $ Text.drop (codePoint loc - ws) input

detokenize :: Text -> [SyntacticToken] -> Text
detokenize trailingSpace ts = Text.concat $ List.concatMap (\t -> [leadingSpace t, lexeme t]) ts <> [trailingSpace]
