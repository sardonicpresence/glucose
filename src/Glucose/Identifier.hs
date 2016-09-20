module Glucose.Identifier where

import Data.Text
import Glucose.Lexer.Char
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

newtype Identifier = Identifier { identify :: Text } deriving (Eq, Ord)

instance Show Identifier where
  show (Identifier n) = unpack n

class Bound a where
  identifier :: a -> Identifier

instance Bound Identifier where
  identifier = id

instance Arbitrary Identifier where
  arbitrary = fmap (Identifier . pack) . (:)
    <$> arbitrary `suchThat` (\c -> isIdentifier c && not (isDigit c))
    <*> listOf (arbitrary `suchThat` isIdentifier)
