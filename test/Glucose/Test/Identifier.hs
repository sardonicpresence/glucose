module Glucose.Test.Identifier ( module Glucose.Identifier ) where

import Data.String (fromString)
import Glucose.Identifier
import Glucose.Lexer.Char

import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

instance Arbitrary Identifier where
  arbitrary = fmap fromString . (:)
    <$> arbitrary `suchThat` (\c -> isIdentifier c && not (isDigit c))
    <*> listOf (arbitrary `suchThat` isIdentifier)
