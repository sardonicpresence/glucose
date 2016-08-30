module Glucose.Parser (parse) where

import Data.Bifunctor
import Data.List.NonEmpty
import Data.Set as Set
import Text.Megaparsec hiding (Token, parse)

import Glucose.AST as AST
import Glucose.Lexer.Lexeme (SyntacticToken)
import Glucose.Lexer.Location
import qualified Glucose.Lexer.Lexeme as Lexer
import Glucose.Token as Token
import Glucose.Parser.Tokens ()

type Parser e = Parsec e [SyntacticToken]

parse :: [SyntacticToken] -> Either String Module
parse = bimap parseErrorPretty id . runParser parser "" where
  parser :: Parser Dec Module
  parser = Module <$> many definition <* eof

definition :: ErrorComponent e => Parser e Definition
definition = do
  name <- identifier
  (SourcePos _ sl sc) <- getPosition
  value <- operator Assign *> literal <* endOfDefinition
  pure $ Definition name value (Location 0 (fromIntegral $ unPos sl) (fromIntegral $ unPos sc))
-- definition = Definition <$> identifier <*> (operator Assign *> literal <* endOfDefinition) <?> "definition"

endOfDefinition :: ErrorComponent e => Parser e ()
endOfDefinition = (eof <|>) . lexeme "end of definition" $ \case
  EndOfDefinition -> Just ()
  _ -> Nothing

identifier :: ErrorComponent e => Parser e Identifier
identifier  = lexeme "identifier" $ \case
  Token.Identifier s -> Just $ AST.Identifier s
  _ -> Nothing

operator :: ErrorComponent e => Token.Operator -> Parser e ()
operator op = lexeme (show op) $ \case
  Token.Operator a -> if a == op then Just () else Nothing
  _ -> Nothing

literal :: ErrorComponent e => Parser e Literal
literal = lexeme "literal" $ \case
  Token.IntegerLiteral n -> Just $ AST.IntegerLiteral (fromInteger n)
  Token.FloatLiteral n -> Just $ AST.FloatLiteral (fromRational n)
  _ -> Nothing

lexeme :: ErrorComponent e => String -> (Token -> Maybe a) -> Parser e a
lexeme name test = token testToken Nothing <?> name where
  testToken x = maybe (Left (Set.singleton (Tokens (x:|[])), Set.empty, Set.empty)) Right $ test (Lexer.token x)
