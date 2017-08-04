module Glucose.Parser.ParseError where

import Data.Semigroup
import Data.Text
import Glucose.Parser.EOFOr
import Glucose.Source
import Glucose.Token

data ParseError = ParseError Location ParseErrorDetails deriving (Eq)

data ParseErrorDetails = ParseErrorDetails { unexpected :: EOFOr (FromSource Token), expected :: [EOFOr Text] }
  deriving (Eq)

instance Semigroup ParseError where
  a <> b | location a > location b = a
  a <> b | location b > location a = b
  (ParseError loc a) <> (ParseError _ b) = ParseError loc (a <> b)

instance Semigroup ParseErrorDetails where
  (ParseErrorDetails unexpected e1) <> (ParseErrorDetails _ e2) = ParseErrorDetails unexpected (e1 <> e2)

instance Located ParseError where
  location (ParseError loc _) = loc
