module Glucose.ParserSpec (spec) where

import Test.Prelude

import Control.Monad
import Data.Text (Text)
import qualified Glucose.AST as AST
import Glucose.Identifier
import Glucose.Lexer
import Glucose.Parser
import Glucose.Parser.EOFOr
import Glucose.Parser.ParseError
import Glucose.Source
import qualified Glucose.Token as Token
import Glucose.Test.AST

spec :: Spec
spec = describe "parse" $ do
  it "parses no tokens to an empty module" $
    parse beginning [] `shouldBe` (Right (AST.Module []) :: Either ParseError (AST.Module FromSource))
  it "errors on incomplete definition" $
    parseTokens "a=" `shouldErrorWith` unexpectedEof "1:3@2" expectedExpression
  it "parses global numeric literal definitions correctly" $
    let a = Identifier "a" `at` "1:1@0"
        valueA = integerLiteral 123 `at` "1:3@2 - 1:5@4"
        b = Identifier "b" `at` "2:1@6"
        valueB = floatLiteral 0.98 `at` "2:3@8 - 2:6@11"
     in parseTokens "a=123\nb=0.98" `shouldParseAs`
          [ AST.Definition a valueA `at` "1:1@0 - 1:5@4"
          , AST.Definition b valueB `at` "2:1@6 - 2:6@11"]
  it "parses global constant aliases correctly" $
    let a = Identifier "a" `at` "1:1@0"
        b = variable "b" `at` "1:3@2"
     in parseTokens "a=b" `shouldParseAs` [AST.Definition a b `at` "1:1@0 - 1:3@2"]
  it "parses definition at eof without newline" $
    let a = Identifier "a" `at` "1:1@0"
        value = integerLiteral 123 `at` "1:3@2 - 1:5@4"
     in parseTokens "a=123" `shouldParseAs` [AST.Definition a value `at` "1:1@0 - 1:5@4"]
  it "errors on superfluous tokens after definition" $
    let unexpected = Token.Operator Token.Arrow `at` "1:7@6 - 1:8@7"
     in parseTokens "a=123 ->" `shouldErrorWith` unexpectedTokenAfter unexpected expectedAfterExpression
  it "parses trivial type definition" $
    let a = Identifier "a" `at` "1:6@5"
        b = Identifier "b" `at` "1:8@7"
     in parseTokens "type a=b" `shouldParseAs` [AST.TypeDefinition a [b] `at` "1:1@0 - 1:8@7"]
  it "parses enum type definition" $
    let a = Identifier "a" `at` "1:6@5"
        b = Identifier "b" `at` "1:8@7"
        c = Identifier "c" `at` "1:10@9"
        d = Identifier "d" `at` "1:12@11"
     in parseTokens "type a=b|c|d" `shouldParseAs` [AST.TypeDefinition a [b,c,d] `at` "1:1@0 - 1:12@11"]
  it "errors on incomplete type definition" $ do
    parseTokens "type" `shouldErrorWith` unexpectedEof "1:5@4" ["identifier"]
    parseTokens "type a" `shouldErrorWith` unexpectedEof "1:7@6" ["\"=\""]
    parseTokens "type a=" `shouldErrorWith` unexpectedEof "1:8@7" ["identifier"]
    parseTokens "type a=b|" `shouldErrorWith` unexpectedEof "1:10@9" ["identifier"]
  it "parses single-argument function definition" $
    let a = Identifier "a" `at` "1:1@0"
        b = Identifier "b" `at` "1:4@3"
        c = variable "c" `at` "1:7@6"
     in parseTokens "a=\\b->c" `shouldParseAs` [AST.Definition a (lambda b c `at` "1:3@2 - 1:7@6") `at` "1:1@0-1:7@6"]
  it "errors on multi-argument lambda" $
    let unexpected = Token.Identifier "c" `at` "1:6@5"
     in parseTokens "a=\\b c->d" `shouldErrorWith` unexpectedToken unexpected ["\"->\""]
  it "errors on no-argument lambda" $
    let unexpected = Token.Operator Token.Arrow `at` "1:5@4 - 1:6@5"
     in parseTokens "a=\\ ->d" `shouldErrorWith` unexpectedToken unexpected ["identifier"]
  it "errors on lambda at global scope" $
    let unexpected = Token.BeginLambda `at` "1:1@0"
     in parseTokens "\\a->b" `shouldErrorWith` unexpectedTokenAfter unexpected expectedAtGlobalScope
  it "errors on malformed lambda" $ do
    let unexpected = (Token.Operator (Token.CustomOperator "%") `at` "1:4@3")
     in parseTokens "a=\\% ->b" `shouldErrorWith` unexpectedToken unexpected ["identifier"]
    let unexpected = (Token.Operator Token.Arrow `at` "1:10@9-1:11@10")
     in parseTokens "a=\\b -> c->d" `shouldErrorWith` unexpectedTokenAfter unexpected expectedAfterExpression
    parseTokens "a=\\  " `shouldErrorWith` unexpectedEof "1:6@5" ["identifier"]
    parseTokens "a=\\b " `shouldErrorWith` unexpectedEof "1:6@5" ["\"->\""]
    parseTokens "a=\\b ->" `shouldErrorWith` unexpectedEof "1:8@7" expectedExpression

parseTokens :: Text -> Either ParseError (AST.Module FromSource)
parseTokens = uncurry parse <=< failOnError . tokenise

shouldParseAs :: Either ParseError (AST.Module FromSource) -> [FromSource (AST.Definition FromSource)] -> Expectation
a `shouldParseAs` defs = a `shouldBe` Right (AST.Module defs)

unexpectedEof :: String -> [Text] -> ParseError
unexpectedEof loc = ParseError (read loc) . ParseErrorDetails EOF . map NotEOF

unexpectedToken :: FromSource Token.Token -> [Text] -> ParseError
unexpectedToken t es = ParseError (startLocation t) . ParseErrorDetails (NotEOF t) $ map NotEOF es
--
unexpectedTokenAfter :: FromSource Token.Token -> [Text] -> ParseError
unexpectedTokenAfter t es = ParseError (startLocation t) . ParseErrorDetails (NotEOF t) $ map NotEOF es ++ [EOF]--

expectedAtGlobalScope, expectedExpression, expectedAfterExpression :: [Text]
expectedAtGlobalScope = ["identifier", "\"type\""]
expectedExpression = ["identifier","literal","lambda"]
expectedAfterExpression = expectedExpression ++ ["end of definition"]
