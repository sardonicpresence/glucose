module Glucose.IR where

import Glucose.Identifier

data Module = Module [Definition] deriving (Eq, Show)

data Definition = Definition Identifier Expression deriving (Eq, Show)

data Expression = Literal Literal deriving (Eq, Show)

data Literal = IntegerLiteral Int | FloatLiteral Double deriving (Eq, Show)

data Type = Integer | Float deriving (Eq, Show)
