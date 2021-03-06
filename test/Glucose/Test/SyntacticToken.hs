module Glucose.Test.SyntacticToken where

import Data.List as List
import Data.Text as Text
import Glucose.Lexer.Char
import Glucose.Lexer.Location
import Glucose.Lexer.SyntacticToken
import Glucose.Token
import Numeric
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

newtype SyntacticTokens = SyntacticTokens [SyntacticToken] deriving (Show)

instance Arbitrary SyntacticTokens where
  arbitrary = SyntacticTokens <$> arbitraryTokens `suchThat` (not . endsLastDefinition) where
    endsLastDefinition [] = True
    endsLastDefinition as = token (List.last as) == EndOfDefinition

newtype WhiteSpace = WhiteSpace Text deriving (Show)

instance Arbitrary WhiteSpace where
  arbitrary = WhiteSpace <$> arbitraryWhitespace

arbitraryTokens :: Gen [SyntacticToken]
arbitraryTokens = scale (`div` 10) . sized $ \n -> choose (0, n) >>= go Nothing where
  go _ 0 = pure []
  go prev n = do
    next <- arbitraryAfter prev
    (next :) <$> go (Just next) (n-1)

arbitraryAfter :: Maybe SyntacticToken -> Gen SyntacticToken
arbitraryAfter prev = do
  let cantEndDefinition = maybe True (EndOfDefinition ==) $ token <$> prev
  newToken <- if cantEndDefinition then arbitrary `suchThat` (/= EndOfDefinition) else arbitrary
  whitespace <- arbitraryWhitespaceBetween (token <$> prev) newToken
  let preceeding = maybe whitespace (\p -> lexeme p `append` whitespace) prev
  let start = Text.foldl' (flip updateLocation) (maybe beginning location prev) preceeding
  newLexeme <- arbitraryRepresentation newToken
  pure $ SyntacticToken newToken start newLexeme whitespace

arbitraryWhitespaceBetween :: Maybe Token -> Token -> Gen Text
arbitraryWhitespaceBetween (Just EndOfDefinition) _ = arbitraryWhitespace `suchThat` endsDefinition
arbitraryWhitespaceBetween Nothing _ = pure empty
arbitraryWhitespaceBetween _ EndOfDefinition = pure empty
arbitraryWhitespaceBetween (Just a) b | requiresGap a b = arbitraryWhitespace1 `suchThat` (not . endsDefinition)
arbitraryWhitespaceBetween _ _ = arbitraryWhitespace `suchThat` (not . endsDefinition)

endsDefinition :: Text -> Bool
endsDefinition a | Text.null a = False
endsDefinition a = isNewline (Text.last a)

requiresGap :: Token -> Token -> Bool
requiresGap (Operator _) (Operator _) = True
requiresGap a b = isAlphanumeric a && isAlphanumeric b

isAlphanumeric :: Token -> Bool
isAlphanumeric (Identifier _) = True
isAlphanumeric (Keyword _) = True
isAlphanumeric (IntegerLiteral _) = True
isAlphanumeric (FloatLiteral _) = True
isAlphanumeric _ = False

arbitraryWhitespace :: Gen Text
arbitraryWhitespace = pack <$> listOf (arbitrary `suchThat` isSpace)

arbitraryWhitespace1 :: Gen Text
arbitraryWhitespace1 = pack <$> listOf1 (arbitrary `suchThat` isSpace)

arbitraryRepresentation :: Token -> Gen Text
arbitraryRepresentation EndOfDefinition = pure empty
arbitraryRepresentation BeginLambda = pure "\\"
arbitraryRepresentation (Identifier a) = pure a
arbitraryRepresentation (Keyword keyword) = pure . pack $ show keyword
arbitraryRepresentation (Operator op) = pure . pack $ show op
arbitraryRepresentation (IntegerLiteral a) = do
  e <- choose (0, maxExponent a)
  let ePart = if e == 0 then "" else "e" ++ show e
      base = a `div` 10^e
  pure . pack $ show base ++ ePart
arbitraryRepresentation (FloatLiteral a) = do
  e <- choose (-10, 10) :: Gen Int
  let ePart = if e == 0 then "" else "e" ++ show e
      base = a / 10^^e
  pure . pack $ showFFloatAlt Nothing (fromRational base :: Double) ePart

maxExponent :: Integer -> Int
maxExponent 0 = 10 -- arbitrary
maxExponent a = go 0 where
  go e = if a `mod` 10^(e+1) == 0 then go (e+1) else e
