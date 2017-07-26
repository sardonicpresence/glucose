module Glucose.Token where

import Control.Lens.Prism
import Data.Text

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
  deriving (Eq, Ord)

instance Show Token where
  show EndOfDefinition = "\n"
  show BeginLambda = "\\"
  show (Identifier a) = show a
  show (Keyword a) = show a
  show (Operator a) = show a
  show OpenParen = "("
  show CloseParen = ")"
  show (IntegerLiteral a) = show a
  show (FloatLiteral a) = show a

_endOfDefinition :: Prism' Token ()
_endOfDefinition = prism' (const EndOfDefinition) $ \case EndOfDefinition -> Just (); _ -> Nothing

_beginLambda :: Prism' Token ()
_beginLambda = prism' (const BeginLambda) $ \case BeginLambda -> Just (); _ -> Nothing

_identifier :: Prism' Token Text
_identifier = prism' Identifier $ \case Identifier a -> Just a; _ -> Nothing

_keyword :: Prism' Token Keyword
_keyword = prism' Keyword $ \case Keyword a -> Just a; _ -> Nothing

_operator :: Prism' Token Operator
_operator = prism' Operator $ \case Operator a -> Just a; _ -> Nothing

_openParen :: Prism' Token ()
_openParen = prism' (const OpenParen) $ \case OpenParen -> Just (); _ -> Nothing

_closeParen :: Prism' Token ()
_closeParen = prism' (const CloseParen) $ \case CloseParen -> Just (); _ -> Nothing

_integerLiteral :: Prism' Token Integer
_integerLiteral = prism' IntegerLiteral $ \case IntegerLiteral a -> Just a; _ -> Nothing

_floatLiteral :: Prism' Token Rational
_floatLiteral = prism' FloatLiteral $ \case FloatLiteral a -> Just a; _ -> Nothing
