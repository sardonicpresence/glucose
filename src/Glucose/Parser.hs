module Glucose.Parser (parse) where

import Control.Applicative hiding ((<**>))
import Control.Comonad
import Control.Lens hiding (traverse1)
import Control.Monad
import Control.Monad.Except
import Data.Maybe
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
  ann <- optional $ typeAnnotation <* endOfDefinition
  when (isJust ann) $ do
    name' <- identifier
    unless (extract name == extract name') . error $ "Type annotation for " <> show (extract name) <> " not immediately followed by definition"
  expr <- operator Assign *> expression
  pure $ (AST.Definition <$> duplicate name <*> duplicate expr) `maybeApply` ann

typeDefinition :: Source f => ParseCompound f AST.Definition
typeDefinition = AST.TypeDefinition <$$ keyword Type <**> name <**> constructors where
  name = duplicate <$> identifier <* operator Assign
  constructors = traverse1 duplicate <$> identifier `separatedBy` operator Bar

expression :: Source f => ParseCompound f AST.Expression
expression = buildExpression <$> some value where
  buildExpression [expr] = AST.Value <$> expr
  buildExpression es = let (a:as) = reverse es in AST.Apply <$> duplicate (buildExpression $ reverse as) <*> duplicate a

value :: Source f => ParseCompound f AST.Value
value = AST.Variable <$$> identifier
    <|> AST.Literal <$$> literal
    <|> toLambda <$> (beginLambda *> some identifier <* operator Arrow) <*> expression
  where
    toLambda a b = AST.Lambda <$> sequence1 (duplicate <$> a) <*> duplicate b

typeAnnotation :: Source f => ParseLexeme f Type
typeAnnotation = operator Colon *> typeR

typeNR :: Source f => ParseLexeme f AST.Type
typeNR = inParens typeR <|> Bound <$$> identifier -- TODO: other types

typeR :: Source f => ParseLexeme f AST.Type
typeR = Function <$$> typeNR <* operator Arrow <**> typeR <|> typeNR

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

inParens :: Traversable f => ParseLexeme f a -> ParseLexeme f a
inParens p = openParen *> p <* closeParen

openParen :: Traversable f => ParseLexeme f ()
openParen = lexeme "open parenthesis" . traverse $ is _openParen

closeParen :: Traversable f => ParseLexeme f ()
closeParen = lexeme "close parenthesis" . traverse $ is _closeParen

-- * Utilities

infixl 4 <$$>, <$$, <**>

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = (<$>) . (<$>)

(<$$) :: (Functor f, Functor g) => a -> f (g b) -> f (g a)
a <$$ b = const a <$$> b

(<**>) :: (Applicative f, Applicative g) => f (g (a -> b)) -> f (g a) -> f (g b)
f <**> a = (<*>) <$> f <*> a

is :: Getting (First ()) s a -> s -> Maybe ()
is a = preview $ a . like ()

maybeApply :: (Applicative f, Comonad f) => f (Maybe (f a) -> b) -> Maybe (f a) -> f b
maybeApply f Nothing = ($ Nothing) <$> f
maybeApply f (Just a) = f <*> (Just <$> duplicate a)
