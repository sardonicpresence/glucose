module Glucose.Token where

import Data.Text

data Operator
  = Assign
  | CustomOperator Text
  deriving (Eq, Ord)

instance Show Operator where
  show Assign = "'='"
  show (CustomOperator s) = "'" ++ unpack s ++ "'"

data Token
  = EndOfDefinition
  | Identifier Text
  | Operator Operator
  | IntegerLiteral Integer
  | FloatLiteral Rational
  deriving (Eq, Ord, Show)
