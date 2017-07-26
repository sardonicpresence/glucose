module Glucose.AST where

import Control.Comonad
import Data.Text
import Glucose.Identifier
import Glucose.Source

newtype Module = Module [FromSource Definition] deriving (Eq, Show)

data Definition = Definition (FromSource Identifier) (FromSource Expression) (Maybe (FromSource Type))
                | TypeDefinition (FromSource Identifier) [FromSource Identifier]
  deriving (Eq, Show)

instance Bound Definition where
  identifier (Definition name _ _) = extract name
  identifier (TypeDefinition name _) = extract name

data Value
  = Literal Literal
  | Variable Identifier
  | Lambda [FromSource Identifier] (FromSource Expression)
  deriving (Eq, Show)

data Expression
  = Value Value
  | Apply (FromSource Expression) (FromSource Value)
  deriving (Eq, Show)

data Literal = IntegerLiteral Int | FloatLiteral Double deriving (Eq, Show)

data Type = Integer | Float | ADT Identifier | Function Type Type | Bound Identifier deriving (Eq)

instance Show Type where
  show Integer = "int"
  show Float = "float"
  show (ADT name) = show name
  show (Function a@Function{} b) = "(" ++ show a ++ ")->" ++ show b
  show (Function a b) = show a ++ "->" ++ show b
  show (Bound name) = show name

integerLiteral :: Int -> Value
integerLiteral = Literal . IntegerLiteral

floatLiteral :: Double -> Value
floatLiteral = Literal . FloatLiteral

variable :: Text -> Value
variable = Variable . Identifier
