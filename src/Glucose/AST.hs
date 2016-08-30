module Glucose.AST where

import Data.Text
import Glucose.Lexer.Location

data Module = Module [Definition] deriving (Eq, Show)

data Definition = Definition Identifier Literal Location deriving (Eq, Show)

data Literal = IntegerLiteral Int | FloatLiteral Double deriving (Eq, Show)

newtype Identifier = Identifier Text deriving (Eq, Ord)

instance Show Identifier where
  show (Identifier n) = show n
