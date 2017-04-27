module Glucose.Token where

import Control.Lens.Prism
import Data.Ratio
import Data.Text
import Glucose.Identifier (identify)
import Glucose.Lexer.Char
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

data Keyword
  = Type
  deriving (Eq, Ord)

instance Show Keyword where
  show Type = "type"

data Operator
  = Assign
  | Colon
  | Arrow
  | Bar
  | CustomOperator Text
  deriving (Eq, Ord)

instance Show Operator where
  show Assign = "="
  show Colon = ":"
  show Arrow = "->"
  show Bar = "|"
  show (CustomOperator s) = unpack s

data Token
  = EndOfDefinition
  | BeginLambda
  | Identifier Text
  | Keyword Keyword
  | Operator Operator
  | OpenParen
  | CloseParen
  | IntegerLiteral Integer
  | FloatLiteral Rational
  deriving (Eq, Ord, Show)

instance Arbitrary Keyword where
  arbitrary = oneof [ pure Type ]

instance Arbitrary Operator where
  arbitrary = oneof
    [ pure Assign
    , pure Arrow
    , pure Bar
    , CustomOperator . pack <$> (listOf1 (arbitrary `suchThat` isOperator) `suchThat` (not . flip elem ["=", ":", "->", "|"])) ]

instance Arbitrary Token where
  arbitrary = oneof
    [ pure EndOfDefinition
    , pure BeginLambda
    , Identifier . identify <$> arbitrary
    , Keyword <$> arbitrary
    , Operator <$> arbitrary
    , pure OpenParen
    , pure CloseParen
    , IntegerLiteral <$> arbitrary `suchThat` (>=0)
    , FloatLiteral <$> do
        base <- arbitrary `suchThat` (>=0)
        e <- arbitrary `suchThat` (>=0) :: Gen Integer
        pure (base % 10^e)
    ]

_endOfDefinition :: Prism' Token ()
_endOfDefinition = prism' (const EndOfDefinition) $ \case EndOfDefinition -> Just (); _ -> Nothing

_beginLambda :: Prism' Token ()
_beginLambda = prism' (const BeginLambda) $ \case BeginLambda -> Just (); _ -> Nothing

_openParen :: Prism' Token ()
_openParen = prism' (const OpenParen) $ \case OpenParen -> Just (); _ -> Nothing

_closeParen :: Prism' Token ()
_closeParen = prism' (const CloseParen) $ \case CloseParen -> Just (); _ -> Nothing

_identifier :: Prism' Token Text
_identifier = prism' Identifier $ \case Identifier a -> Just a; _ -> Nothing

_keyword :: Prism' Token Keyword
_keyword = prism' Keyword $ \case Keyword a -> Just a; _ -> Nothing

_operator :: Prism' Token Operator
_operator = prism' Operator $ \case Operator a -> Just a; _ -> Nothing

_integerLiteral :: Prism' Token Integer
_integerLiteral = prism' IntegerLiteral $ \case IntegerLiteral a -> Just a; _ -> Nothing

_floatLiteral :: Prism' Token Rational
_floatLiteral = prism' FloatLiteral $ \case FloatLiteral a -> Just a; _ -> Nothing
