module Glucose.Token where

import Control.Lens.Prism
import Data.Ratio
import Data.Text
import Glucose.Identifier (identify)
import Glucose.Lexer.Char
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

data Operator
  = Assign
  | Arrow
  | CustomOperator Text
  deriving (Eq, Ord)

instance Show Operator where
  show Assign = "="
  show Arrow = "->"
  show (CustomOperator s) = unpack s

_assign :: Prism' Operator ()
_assign = prism' undefined $ \case Assign -> Just (); _ -> Nothing

data Token
  = EndOfDefinition
  | BeginLambda
  | Identifier Text
  | Operator Operator
  | IntegerLiteral Integer
  | FloatLiteral Rational
  deriving (Eq, Ord, Show)

instance Arbitrary Operator where
  arbitrary = oneof
    [ pure Assign
    , pure Arrow
    , CustomOperator . pack <$> (listOf1 (arbitrary `suchThat` isOperator) `suchThat` (not . flip elem ["=", "->"])) ]

instance Arbitrary Token where
  arbitrary = oneof
    [ pure EndOfDefinition
    , pure BeginLambda
    , Identifier . identify <$> arbitrary
    , Operator <$> arbitrary
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

_identifier :: Prism' Token Text
_identifier = prism' Identifier $ \case Identifier a -> Just a; _ -> Nothing

_operator :: Prism' Token Operator
_operator = prism' Operator $ \case Operator a -> Just a; _ -> Nothing

_integerLiteral :: Prism' Token Integer
_integerLiteral = prism' IntegerLiteral $ \case IntegerLiteral a -> Just a; _ -> Nothing

_floatLiteral :: Prism' Token Rational
_floatLiteral = prism' FloatLiteral $ \case FloatLiteral a -> Just a; _ -> Nothing
