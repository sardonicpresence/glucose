module Glucose.Parser.ParseError where

import Data.Semigroup
import Data.Text
import Glucose.Parser.EOFOr
import Glucose.Source
import Glucose.Token

data ParseError = ParseError Location ParseErrorDetails deriving (Eq, Show)

data ParseErrorDetails = ParseErrorDetails { unexpected :: EOFOr (FromSource Token), expected :: [EOFOr Text] }
  deriving (Eq, Show)

instance Ord ParseError where
  compare (ParseError a _) (ParseError b _) = compare a b

instance Semigroup ParseError where
  a <> b | a > b = a
  a <> b | b > a = b
  (ParseError loc a) <> (ParseError _ b) = ParseError loc (a <> b)

instance Semigroup ParseErrorDetails where
  (ParseErrorDetails unexpected e1) <> (ParseErrorDetails _ e2) = ParseErrorDetails unexpected (e1 <> e2)
