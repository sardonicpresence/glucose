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
    parseTokens "a=" `shouldErrorWith` unexpectedEof "1:3@2" ["identifier","literal","lambda"]
  it "parses global numeric literal definitions correctly" $
    let a = Identifier "a" `at` "1:1@0 - 1:1@0"
        valueA = integerLiteral 123 `at` "1:3@2 - 1:5@4"
        b = Identifier "b" `at` "2:1@6 - 2:1@6"
        valueB = floatLiteral 0.98 `at` "2:3@8 - 2:6@11"
     in parseTokens "a=123\nb=0.98" `shouldParseAs`
          [ AST.Definition a valueA Nothing `at` "1:1@0 - 1:5@4"
          , AST.Definition b valueB Nothing `at` "2:1@6 - 2:6@11"]
  it "parses global constant aliases correctly" $
    let a = Identifier "a" `at` "1:1@0 - 1:1@0"
        b = variable "b" `at` "1:3@2 - 1:3@2"
     in parseTokens "a=b" `shouldParseAs` [AST.Definition a b Nothing `at` "1:1@0 - 1:3@2"]
  it "parses definition at eof without newline" $
    let a = Identifier "a" `at` "1:1@0 - 1:1@0"
        value = integerLiteral 123 `at` "1:3@2 - 1:5@4"
     in parseTokens "a=123" `shouldParseAs` [AST.Definition a value Nothing `at` "1:1@0 - 1:5@4"]
  it "errors on superfluous tokens after definition" $
    let unexpected = Token.Operator Token.Arrow `at` "1:7@6 - 1:8@7"
     in parseTokens "a=123 ->" `shouldErrorWith` unexpectedToken unexpected ["identifier","literal","lambda","end of definition"]
  it "parses trivial type definition" $
    let a = Identifier "a" `at` "1:6@5 - 1:6@5"
        b = Identifier "b" `at` "1:8@7 - 1:8@7"
     in parseTokens "type a=b" `shouldParseAs` [AST.TypeDefinition a [b] `at` "1:1@0 - 1:8@7"]
  it "parses enum type definition" $
    let a = Identifier "a" `at` "1:6@5 - 1:6@5"
        b = Identifier "b" `at` "1:8@7 - 1:8@7"
        c = Identifier "c" `at` "1:10@9 - 1:10@9"
        d = Identifier "d" `at` "1:12@11 - 1:12@11"
     in parseTokens "type a=b|c|d" `shouldParseAs` [AST.TypeDefinition a [b,c,d] `at` "1:1@0 - 1:12@11"]
  it "errors on incomplete type definition" $ do
    parseTokens "type" `shouldErrorWith` unexpectedEof "1:5@4" ["identifier"]
    parseTokens "type a" `shouldErrorWith` unexpectedEof "1:7@6" ["\"=\""]
    parseTokens "type a=" `shouldErrorWith` unexpectedEof "1:8@7" ["identifier"]
    parseTokens "type a=b|" `shouldErrorWith` unexpectedEof "1:10@9" ["identifier"]

parseTokens :: Text -> Either ParseError (AST.Module FromSource)
parseTokens = uncurry parse <=< failOnError . tokenise

shouldParseAs :: Either ParseError (AST.Module FromSource) -> [FromSource (AST.Definition FromSource)] -> Expectation
a `shouldParseAs` defs = a `shouldBe` Right (AST.Module defs)

unexpectedEof :: String -> [Text] -> ParseError
unexpectedEof loc = ParseError (read loc) . ParseErrorDetails EOF . map NotEOF

unexpectedToken :: FromSource Token.Token -> [Text] -> ParseError
unexpectedToken t es = ParseError (startLocation t) . ParseErrorDetails (NotEOF t) $ map NotEOF es ++ [EOF]
--
