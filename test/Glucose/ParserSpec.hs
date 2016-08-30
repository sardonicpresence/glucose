module Glucose.ParserSpec (spec) where

import Test.Prelude

import qualified Glucose.AST as AST
import Glucose.Lexer.Lexeme
import Glucose.Lexer.Location
import Glucose.Parser
import Glucose.Token

spec :: Spec
spec = describe "parse" $ do
  it "parses no tokens to an empty module" $
    parse [] `shouldBe` Right (AST.Module [])
  it "errors on incomplete definition" $
    parseTokens [Identifier "a", Operator Assign] `shouldErrorContaining` "expect"
  it "parses global numeric literal definitions correctly" $
    parseTokens [Identifier "a", Operator Assign, IntegerLiteral 123, EndOfDefinition,
                 Identifier "b", Operator Assign, FloatLiteral 0.98, EndOfDefinition] `shouldBe`
      Right (AST.Module [AST.Definition (AST.Identifier "a") (AST.IntegerLiteral 123) beginning,
                         AST.Definition (AST.Identifier "b") (AST.FloatLiteral 0.98) beginning])
  it "parses definition at eof without newline" $
    parseTokens [Identifier "a", Operator Assign, IntegerLiteral 123] `shouldBe`
      Right (AST.Module [AST.Definition (AST.Identifier "a") (AST.IntegerLiteral 123) beginning])
  it "errors on superfluous tokens after definition" $
    parseTokens [Identifier "a", Operator Assign, IntegerLiteral 123, Identifier "b"]
      `shouldErrorContaining` "expecting end"

parseTokens :: [Token] -> Either String AST.Module
parseTokens = parse . map (\t -> SyntacticToken t beginning "")
