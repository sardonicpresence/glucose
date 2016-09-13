module Glucose.AST where

import Glucose.Identifier
import Glucose.Lexer.Location

data Module = Module [Definition] deriving (Eq, Show)

data Definition = Definition Identifier Expression Location deriving (Eq, Show)

data Expression = Literal Literal | Variable Identifier deriving (Eq, Show)

data Literal = IntegerLiteral Int | FloatLiteral Double deriving (Eq, Show)
