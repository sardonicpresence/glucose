module Glucose.AST where

import Control.Comonad
import Glucose.Identifier

newtype Module f = Module [f (Definition f)]

deriving instance Eq (f (Definition f)) => Eq (Module f)
deriving instance Show (f (Definition f)) => Show (Module f)

data Definition f = Definition (f Identifier) (f (Expression f))
                  | TypeDefinition (f Identifier) [f Identifier]

deriving instance (Eq (f (Expression f)), Eq (f Identifier), Eq (f Type)) => Eq (Definition f)
deriving instance (Show (f (Expression f)), Show (f Identifier), Show (f Type)) => Show (Definition f)

instance Comonad f => Bound f (Definition f) where
  identifier (Definition name _) = name
  identifier (TypeDefinition name _) = name

data Value f
  = Literal Literal
  | Variable Identifier
  | Lambda (f Identifier) (f (Expression f))

deriving instance (Eq (f (Expression f)), Eq (f Identifier)) => Eq (Value f)
deriving instance (Show (f (Expression f)), Show (f Identifier)) => Show (Value f)

data Expression f
  = Value (Value f)
  | Apply (f (Expression f)) (f (Value f))

deriving instance (Eq (f (Expression f)), Eq (f (Value f)), Eq (Value f)) => Eq (Expression f)
deriving instance (Show (f (Expression f)), Show (f (Value f)), Show (Value f)) => Show (Expression f)

data Literal = IntegerLiteral Int | FloatLiteral Double deriving (Eq, Show)

data Type = Integer | Float | ADT Identifier | Function Type Type | Bound Identifier deriving (Eq)

instance Show Type where
  show Integer = "int"
  show Float = "float"
  show (ADT name) = show name
  show (Function a@Function{} b) = "(" ++ show a ++ ")->" ++ show b
  show (Function a b) = show a ++ "->" ++ show b
  show (Bound name) = show name
