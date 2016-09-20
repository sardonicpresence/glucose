module Glucose.ParserSpec (spec) where

import Test.Prelude

import Control.Monad
import Data.Text (Text)
import qualified Glucose.AST as AST
import Glucose.Error
import Glucose.Identifier
import Glucose.Lexer
import Glucose.Lexer.Location
import Glucose.Parser
import Glucose.Parser.Source
import qualified Glucose.Token as Token
import Glucose.Test.Error

spec :: Spec
spec = describe "parse" $ do
  it "parses no tokens to an empty module" $
    parse beginning [] `shouldBe` (Right (AST.Module []) :: Either CompileError AST.Module)
  it "errors on incomplete definition" $
    parseTokens "a=" `shouldErrorWith` unexpectedEof "1:3@2" ["identifier","literal"]
  it "parses global numeric literal definitions correctly" $
    let a = Identifier "a" `at` "1:1@0 - 1:1@0"
        valueA = AST.integerLiteral 123 `at` "1:3@2 - 1:5@4"
        b = Identifier "b" `at` "2:1@6 - 2:1@6"
        valueB = AST.floatLiteral 0.98 `at` "2:3@8 - 2:6@11"
     in parseTokens "a=123\nb=0.98" `shouldParseAs`
          [ AST.Definition a valueA `at` "1:1@0 - 1:5@4"
          , AST.Definition b valueB `at` "2:1@6 - 2:6@11"]
  it "parses global constant aliases correctly" $
    let a = Identifier "a" `at` "1:1@0 - 1:1@0"
        b = AST.variable "b" `at` "1:3@2 - 1:3@2"
     in parseTokens "a=b" `shouldParseAs` [AST.Definition a b `at` "1:1@0 - 1:3@2"]
  it "parses definition at eof without newline" $
    let a = Identifier "a" `at` "1:1@0 - 1:1@0"
        value = AST.integerLiteral 123 `at` "1:3@2 - 1:5@4"
     in parseTokens "a=123" `shouldParseAs` [AST.Definition a value `at` "1:1@0 - 1:5@4"]
  it "errors on superfluous tokens after definition" $
    let b = Token.Identifier "b" `at` "1:7@6 - 1:7@6"
     in parseTokens "a=123 b" `shouldErrorWith` unexpectedToken "1:7@6" b ["end of definition","end of file"]

parseTokens :: Text -> Either CompileError AST.Module
parseTokens = uncurry parse <=< tokenise

shouldParseAs :: Either CompileError AST.Module -> [FromSource AST.Definition] -> Expectation
a `shouldParseAs` defs = a `shouldBe` Right (AST.Module defs)
