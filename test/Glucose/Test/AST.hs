module Glucose.Test.AST where

import Data.Text (Text)
import Glucose.AST
import Glucose.Identifier (Identifier(..))

integerLiteral :: Int -> Expression f
integerLiteral = Value . Literal . IntegerLiteral

floatLiteral :: Double -> Expression f
floatLiteral = Value . Literal . FloatLiteral

variable :: Text -> Expression f
variable = Value . Variable . Identifier

lambda :: f Identifier -> f (Expression f) -> Expression f
lambda = (Value .) . Lambda
