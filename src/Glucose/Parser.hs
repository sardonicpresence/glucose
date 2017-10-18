module Glucose.Parser (parse) where

import Control.Applicative hiding ((<**>))
import Control.Comonad
import Control.Lens hiding (traverse1)
import Control.Monad.Except
import Data.Monoid
import Data.Text (pack)

import Glucose.AST as AST
import Glucose.Identifier as AST hiding (identifier)
import Glucose.Parser.EOFOr
import Glucose.Parser.Monad
import Glucose.Parser.ParseError
import Glucose.Source
import Glucose.Token as Token

type Parse f a = Parser ParseError (f Token) [f Token] a
type ParseCompound f a = Parse f (f (a f))
type ParseLexeme f a = Parse f (f a)

type Source f = (Comonad f, Applicative f, Traversable f)

parse :: MonadError ParseError m => Location -> [FromSource Token] -> m (Module FromSource)
parse eofLocation = runParser moduleParser (onError eofLocation)

onError :: Location -> EOFOr (FromSource Token) -> [EOFOr String] -> ParseError
onError eofLocation unexpected expected = ParseError location $ ParseErrorDetails unexpected (map (pack <$>) expected) where
  location = maybeEOF eofLocation startLocation unexpected

moduleParser :: Source f => Parse f (Module f)
moduleParser = Module <$> many ((definition <|> typeDefinition) <* endOfDefinition) <* eof

definition :: Source f => ParseCompound f AST.Definition
definition = do
  name <- identifier
  expr <- operator Assign *> expression
  pure $ AST.Definition <$> duplicate name <*> duplicate expr

typeDefinition :: Source f => ParseCompound f AST.Definition
typeDefinition = AST.TypeDefinition <$$ keyword Type <**> name <**> constructors where
  name = withSource identifier <* operator Assign
  constructors = traverse1 duplicate <$> identifier `separatedBy` operator Bar

expression :: Source f => ParseCompound f AST.Expression
expression = buildExpression <$> some value where
  buildExpression [expr] = AST.Value <$> expr
  buildExpression es = let (a:as) = reverse es in AST.Apply <$> duplicate (buildExpression $ reverse as) <*> duplicate a

value :: forall f. Source f => ParseCompound f AST.Value
value = AST.Variable <$$> identifier
    <|> AST.Literal <$$> literal
    <|> AST.Lambda <$$> (beginLambda **> withSource identifier <** operator Arrow) <**> withSource expression

endOfDefinition :: Source f => ParseLexeme f ()
endOfDefinition = (lexeme "end of definition" . traverse $ is _endOfDefinition) <|> pure <$> eof

identifier :: Traversable f => ParseLexeme f AST.Identifier
identifier = lexeme "identifier" . traverse $ AST.Identifier <$$> preview _identifier

keyword :: Traversable f => Keyword -> ParseLexeme f ()
keyword kw = lexeme ("\"" ++ show kw ++ "\"") . traverse . is $ _keyword . filtered (kw ==)

operator :: Traversable f => Operator -> ParseLexeme f ()
operator op = lexeme ("\"" ++ show op ++ "\"") . traverse . is $ _operator . filtered (op ==)

literal :: Traversable f => ParseLexeme f Literal
literal = lexeme "literal" . traverse $ \t -> integerLiteral t <|> floatLiteral t where
  integerLiteral = AST.IntegerLiteral . fromInteger <$$> preview _integerLiteral
  floatLiteral = AST.FloatLiteral . fromRational <$$> preview _floatLiteral

beginLambda :: Traversable f => ParseLexeme f ()
beginLambda = lexeme "lambda" . traverse $ is _beginLambda

-- * Utilities

infixl 4 <$$>, <$$, <**>, <**, **>

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = (<$>) . (<$>)

(<$$) :: (Functor f, Functor g) => a -> f (g b) -> f (g a)
a <$$ b = const a <$$> b

(<**>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
f <**> a = (<*>) <$> f <*> a

(<**) :: (Applicative f, Applicative g) => f (g a) -> f (g b) -> f (g a)
a <** b = (<*) <$> a <*> b

(**>) :: (Applicative f, Applicative g) => f (g a) -> f (g b) -> f (g b)
a **> b = (*>) <$> a <*> b

is :: Getting (First ()) s a -> s -> Maybe ()
is a = preview $ a . like ()

withSource :: (Functor f, Comonad g) => f (g a) -> f (g (g a))
withSource = fmap duplicate
