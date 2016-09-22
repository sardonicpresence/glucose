module Glucose.Parser where

import Control.Applicative hiding ((<**>))
import Control.Comonad
import Control.Lens hiding (traverse1)
import Data.Monoid

import Glucose.AST as AST
import Glucose.Error
import Glucose.Identifier as AST hiding (identifier)
import Glucose.Lexer.Location
import Glucose.Token as Token
import Glucose.Parser.EOFOr
import Glucose.Parser.Monad
import Glucose.Parser.Source

infixl 4 <$$>, <$$, <**>, <**, **>

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = (<$>) . (<$>)

(<$$) :: (Functor f, Functor g) => a -> f (g b) -> f (g a)
a <$$ b = const a <$$> b

(<**>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
f <**> a = (<*>) <$> f <*> a

(**>) :: (Applicative f, Applicative g) => f (g a) -> f (g b) -> f (g b)
a **> b = flip const <$$> a <**> b

(<**) :: (Applicative f, Applicative g) => f (g a) -> f (g b) -> f (g a)
a <** b = const <$$> a <**> b

type Parse a = Parser Location (FromSource Token) [FromSource Token] (FromSource a)

parse :: (MonadThrow (ParseError Location (FromSource Token)) m) => Location -> [FromSource Token] -> m Module
parse eofLocation = runParser parser (maybeEOF eofLocation startLocation) where
  parser = Module <$> many ((definition <|> typeDefinition) <* endOfDefinition) <* eof

definition :: Parse AST.Definition
definition = AST.Definition <$$> name <**> expr where
  name = duplicate <$> identifier <* operator Assign
  expr = duplicate <$> expression

typeDefinition :: Parse AST.Definition
typeDefinition = AST.TypeDefinition <$$ keyword Type <**> name <**> constructors where
  name = duplicate <$> identifier <* operator Assign
  constructors = traverse1 duplicate <$> identifier `separatedBy` operator Bar

expression :: Parse AST.Expression
expression = buildExpression <$> some value

buildExpression :: [FromSource AST.Value] -> FromSource AST.Expression
buildExpression [expr] = AST.Value <$> expr
buildExpression es = let (a:as) = reverse es in AST.Apply <$> duplicate (buildExpression $ reverse as) <*> duplicate a

value :: Parse AST.Value
value = AST.Variable <$$> identifier
    <|> AST.Literal <$$> literal
    <|> toLambda <$> (beginLambda *> some identifier <* operator Arrow) <*> expression
  where
    toLambda a b = AST.Lambda <$> sequence (duplicate <$> a) <*> duplicate b

endOfDefinition :: Parse ()
endOfDefinition = (lexeme "end of definition" . traverse $ is _endOfDefinition) <|> pure <$> eof

identifier :: Parse AST.Identifier
identifier = lexeme "identifier" . traverse $ AST.Identifier <$$> preview _identifier

keyword :: Keyword -> Parse ()
keyword kw = lexeme ("\"" ++ show kw ++ "\"") . traverse . is $ _keyword . filtered (kw ==)

operator :: Operator -> Parse ()
operator op = lexeme ("\"" ++ show op ++ "\"") . traverse . is $ _operator . filtered (op ==)

literal :: Parse Literal
literal = lexeme "literal" . traverse $ \t -> integerLiteral t <|> floatLiteral t where
  integerLiteral = AST.IntegerLiteral . fromInteger <$$> preview _integerLiteral
  floatLiteral = AST.FloatLiteral . fromRational <$$> preview _floatLiteral

beginLambda :: Parse ()
beginLambda = lexeme "lambda" (traverse $ is _beginLambda)

is :: Getting (First ()) s a -> s -> Maybe ()
is a = preview $ a . like ()
