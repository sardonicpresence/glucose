module Glucose.Parser.ParseError where

import Data.Semigroup
import Data.Text
import Glucose.Parser.EOFOr
import Glucose.Source
import Glucose.Token

type ParseError = Located ParseErrorDetails

data ParseErrorDetails = ParseError { unexpected :: EOFOr (FromSource Token), expected :: [EOFOr Text] }

instance Semigroup ParseErrorDetails where
  (ParseError unexpected e1) <> (ParseError _ e2) = ParseError unexpected (e1 <> e2)
