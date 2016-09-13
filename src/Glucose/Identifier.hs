module Glucose.Identifier where

import Data.Text

newtype Identifier = Identifier Text deriving (Eq, Ord)

instance Show Identifier where
  show (Identifier n) = show n
