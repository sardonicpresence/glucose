module Glucose.Lexer.Lexeme where

import Data.Text as Text
import Glucose.Lexer.Location
import Glucose.Token

data Lexeme = Lexeme (Maybe Token) Location Int
  deriving (Show)

data SyntacticToken = SyntacticToken { token :: Token, location :: Location, lexeme :: Text }
  deriving (Eq, Ord)

instance Show SyntacticToken where
  show (SyntacticToken EndOfDefinition _ s) = (if Text.null s then "implicit" else "explicit") ++ " end of definition"
  show (SyntacticToken (Identifier _) _ s) = "identifier '" ++ unpack s ++ "'"
  show (SyntacticToken (Operator _) _ s) = "operator '" ++ unpack s ++ "'"
  show (SyntacticToken (IntegerLiteral _) _ s) = "integer literal '" ++ unpack s ++ "'"
  show (SyntacticToken (FloatLiteral _) _ s) = "rational literal '" ++ unpack s ++ "'"

syntacticToken :: Text -> Lexeme -> Maybe SyntacticToken
syntacticToken _ (Lexeme Nothing _ _) = Nothing
syntacticToken input (Lexeme (Just token) loc@(Location start _ _) len) = Just $
  SyntacticToken token loc (Text.take len $ Text.drop start input)
