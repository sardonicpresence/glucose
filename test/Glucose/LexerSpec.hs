module Glucose.LexerSpec (spec) where

import Test.Prelude

import Data.Text as Text (pack, foldl)
import Glucose.Error
import Glucose.Lexer
import Glucose.Lexer.Location
import Glucose.Lexer.Reversible hiding (token)
import Glucose.Test.Error
import Glucose.Test.Lexer ()
import Glucose.Token

spec :: Spec
spec = do
  describe "tokens" $ do
    it "parses white-space to no tokens" $
      tokens " \r\n \f\xa0\x85 " `shouldBe` Right []
    it "fails on initial indented definition" $
      tokens " \n a\n " `shouldErrorWith` unexpectedThing "2:2@3" "indentation" "before first definition"
    it "correctly parses a lone identifier" $
      tokens "_a_3b" `shouldBe` Right [Identifier "_a_3b"]
    it "correctly parses an identifier immersed in white-space" $
      tokens " \t\r\nAa3Z_  \x85\f " `shouldBe` Right [Identifier "Aa3Z_"]
    it "correctly parses expressions of the form 'a~b'" $
      tokens "_a_>?_2" `shouldBe` Right [Identifier "_a_", Operator (CustomOperator ">?"), Identifier "_2"]
    it "correctly parses expressions of the form 'a ~ b'" $
      tokens "_a_\xa0\t~~^    _0_" `shouldBe` Right [Identifier "_a_", Operator (CustomOperator "~~^"), Identifier "_0_"]
    it "correctly parses infix = application with no white-space" $
      tokens "_a_=_2" `shouldBe` Right [Identifier "_a_", Operator Assign, Identifier "_2"]
    it "inserts no tokens for indented newline" $
      tokens "\n\na\n  = \n 1 \n " `shouldBe` Right [Identifier "a", Operator Assign, IntegerLiteral 1]
    it "inserts implicit end-of-definition for unindented newlines" $
      tokens "\n\na\n  = \n1 \n b" `shouldBe` Right [Identifier "a", Operator Assign, EndOfDefinition, IntegerLiteral 1, Identifier "b"]
    itParsesKeywords
    itParsesIntegerLiterals
    itParsesFractionalLiterals
    itCorrectlyParsesInfixApplication
  describe "tokeniseReversible" $
    it "is reversable" $ property $ \tokens ->
      let source = detokenise tokens
          eofLocation = Text.foldl (flip updateLocation) beginning source
       in tokeniseReversible source === Right (eofLocation, tokens)

itParsesKeywords :: SpecWith ()
itParsesKeywords = describe "correctly parses keywords" $
  mapM_ correctlyParsesKeyword [Type] where
    correctlyParsesKeyword keyword = itCorrectlyParses ("=" ++ show keyword ++ "=")
      [Operator Assign, Keyword keyword, Operator Assign]

itCorrectlyParsesInfixApplication :: SpecWith ()
itCorrectlyParsesInfixApplication = describe "correctly parses infix application" $
  sequence_ $ concatMap (\op -> map ($op) [itParsesInfix a b | a <- argTypes, b <- argTypes]) $
    map (CustomOperator . pack) ["%~", ".#", "^.", "..", ".-"] ++ [Assign, Arrow, Bar]

itParsesInfix :: Argument -> Argument -> Operator -> SpecWith ()
itParsesInfix a b op = let testOp = show op; tokenOp = Operator op in
  context ("with " ++ explain a ++ " and " ++ explain b ++ " arguments") $ do
    itCorrectlyParses (show a ++ testOp ++ show b) [token a, tokenOp, token b]
    itCorrectlyParses (show a ++ "\xa0 " ++ testOp ++ show b) [token a, tokenOp, token b]
    itCorrectlyParses (show a ++ testOp ++ "\xa0 " ++ show b) [token a, tokenOp, token b]
    itCorrectlyParses (show a ++ " \xa0" ++ testOp ++ " \xa0" ++ show b) [token a, tokenOp, token b]

itCorrectlyParses :: String -> [Token] -> SpecWith (Arg Expectation)
itCorrectlyParses s expected = it (show s) $ tokens (pack s) `shouldBe` Right expected

itParsesIntegerLiterals :: SpecWith ()
itParsesIntegerLiterals = describe "correctly parses integer literals" $ do
  it "correctly parses zero" $
    tokens "0" `shouldBe` Right [IntegerLiteral 0]
  it "correctly parses a simple positive integer literal" $
    tokens "123" `shouldBe` Right [IntegerLiteral 123]
  it "correctly parses a simple negative integer literal" $
    tokens "-321" `shouldBe` Right [Operator (CustomOperator "-"), IntegerLiteral 321]
  it "correctly parses an integer literal with exponent" $
    tokens "12e03" `shouldBe` Right [IntegerLiteral 12000]
  it "correctly parses an integer literal with large exponent" $
    tokens "12e010" `shouldBe` Right [IntegerLiteral 120000000000]
  it "errors on integer literal with blank exponent" $
    tokens "12e" `shouldErrorContaining` "missing exponent"
  it "errors on integer literal with negative exponent" $
    tokens "12e-3" `shouldErrorWith` compileError "1:4@3" (SyntaxError "negative exponent" "integer literal")
  it "errors on unexpected char immediately following integer literal" $
    tokens "123f" `shouldErrorWith` unexpectedChar "1:4@3" 'f' "in numeric literal"
  it "errors on unexpected char within integer literal" $
    tokens "12_3"  `shouldErrorWith` unexpectedChar "1:3@2" '_' "in numeric literal"

itParsesFractionalLiterals :: SpecWith ()
itParsesFractionalLiterals = describe "correctly parses fractional literals" $ do
  it "correctly parses zero" $
    tokens "0.000" `shouldBe` Right [FloatLiteral 0]
  it "correctly parses a simple positive fractional literal" $
    tokens "12.34" `shouldBe` Right [FloatLiteral 12.34]
  it "correctly parses a simple negative fractional literal" $
    tokens "-54.321" `shouldBe` Right [Operator (CustomOperator "-"), FloatLiteral 54.321]
  it "correctly parses a fractional literal with exponent" $
    tokens "12.3e03" `shouldBe` Right [FloatLiteral 12300]
  it "correctly parses a fractional literal with large exponent" $
    tokens "20.01e013" `shouldBe` Right [FloatLiteral 200100000000000]
  it "correctly parses a fractional literal with negative exponent" $
    tokens "9876543.21e-08" `shouldBe` Right [FloatLiteral 0.0987654321]
  it "errors on fractional literal with blank exponent" $
    tokens "12.3e" `shouldErrorWith` compileError "1:6@5" (SyntaxError "missing exponent" "numeric literal")
  it "errors on unexpected char immediately following fractional literal" $
    tokens "123.4f" `shouldErrorWith` unexpectedChar "1:6@5" 'f' "in numeric literal"
  it "errors on unexpected char within fractional literal" $
    tokens "12._3" `shouldErrorWith` unexpectedChar "1:4@3" '_' "in numeric literal"

-- * Argument test-cases

data Argument = Variable | SimpleInt | IntWithExp | SimpleFloat | FloatWithExp

instance Show Argument where
  show Variable = "e"
  show SimpleInt = "10"
  show IntWithExp = "12e03"
  show SimpleFloat = "0.1"
  show FloatWithExp = "3.2e-01"

explain :: Argument -> String
explain Variable = "identifier"
explain SimpleInt = "integer literal"
explain IntWithExp = "integer literal with exponent"
explain SimpleFloat = "fractional literal"
explain FloatWithExp = "fractional literal with exponent"

token :: Argument -> Token
token Variable = Identifier "e"
token SimpleInt = IntegerLiteral 10
token IntWithExp = IntegerLiteral 12000
token SimpleFloat = FloatLiteral 0.1
token FloatWithExp = FloatLiteral 0.32

argTypes :: [Argument]
argTypes = [Variable, SimpleInt, IntWithExp, SimpleFloat, FloatWithExp]
