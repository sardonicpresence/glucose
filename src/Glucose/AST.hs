module Glucose.AST where

import Control.Comonad
import Data.Text
import Glucose.Identifier
import Glucose.Parser.Source

data Module = Module [FromSource Definition] deriving (Eq, Show)

data Definition = Definition (FromSource Identifier) (FromSource Expression) deriving (Eq, Show)

instance Bound Definition where
  identifier (Definition name _) = extract name

data Expression
  = Literal Literal
  | Variable Identifier
  deriving (Eq, Show)

data Literal = IntegerLiteral Int | FloatLiteral Double deriving (Eq, Show)

integerLiteral :: Int -> Expression
integerLiteral = Literal . IntegerLiteral

floatLiteral :: Double -> Expression
floatLiteral = Literal . FloatLiteral

variable :: Text -> Expression
variable = Variable . Identifier
