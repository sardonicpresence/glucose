module Glucose.AST where

import Control.Comonad
import Data.Text
import Glucose.Identifier
import Glucose.Parser.Source

data Module = Module [FromSource Definition] deriving (Eq, Show)

data Definition = Definition (FromSource Identifier) (FromSource Expression) deriving (Eq, Show)

instance Bound Definition where
  identifier (Definition name _) = extract name

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

integerLiteral :: Int -> Value
integerLiteral = Literal . IntegerLiteral

floatLiteral :: Double -> Value
floatLiteral = Literal . FloatLiteral

variable :: Text -> Value
variable = Variable . Identifier
