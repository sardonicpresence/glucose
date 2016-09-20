module Glucose.Parser where

import Control.Applicative
import Control.Comonad
import Control.Lens
import Data.Monoid

import Glucose.AST as AST
import Glucose.Error
import Glucose.Identifier as AST hiding (identifier)
import Glucose.Lexer.Location
import Glucose.Token as Token
import Glucose.Parser.EOFOr
import Glucose.Parser.Monad
import Glucose.Parser.Source

infixl 4 <$$>

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = (<$>) . (<$>)

type Parse a = Parser Location (FromSource Token) [FromSource Token] (FromSource a)

parse :: (MonadThrow (ParseError Location (FromSource Token)) m) => Location -> [FromSource Token] -> m Module
parse eofLocation = runParser parser (maybeEOF eofLocation startLocation) where
  parser = Module <$> many definition <* eof

definition :: Parse AST.Definition
definition = toDefinition <$> identifier <* operator Assign <*> expression <* endOfDefinition where
  toDefinition a b = AST.Definition <$> duplicate a <*> duplicate b

expression :: Parse AST.Expression
expression = AST.Variable <$$> identifier
         <|> AST.Literal <$$> literal

endOfDefinition :: Parse ()
endOfDefinition = (lexeme "end of definition" . traverse $ is _endOfDefinition) <|> pure <$> eof

identifier :: Parse AST.Identifier
identifier = lexeme "identifier" . traverse $ AST.Identifier <$$> preview _identifier

operator :: Operator -> Parse ()
operator op = lexeme "operator" . traverse . is $ _operator . filtered (op ==)

literal :: Parse Literal
literal = lexeme "literal" . traverse $ \t -> integerLiteral t <|> floatLiteral t where
  integerLiteral = AST.IntegerLiteral . fromInteger <$$> preview _integerLiteral
  floatLiteral = AST.FloatLiteral . fromRational <$$> preview _floatLiteral

beginLambda :: Parse ()
beginLambda = lexeme "lambda" (traverse $ is _beginLambda)

is :: Getting (First ()) s a -> s -> Maybe ()
is a = preview $ a . like ()
