module Glucose.Identifier where

import Data.String
import Data.Text
import Glucose.Lexer.Char
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

newtype Identifier = Identifier { identify :: Text } deriving (Eq, Ord)

instance Show Identifier where
  show (Identifier n) = unpack n

instance IsString Identifier where
  fromString = Identifier . pack

class Bound a where
  identifier :: a -> Identifier

instance Bound Identifier where
  identifier = id

instance Arbitrary Identifier where
  arbitrary = fmap fromString . (:)
    <$> arbitrary `suchThat` (\c -> isIdentifier c && not (isDigit c))
    <*> listOf (arbitrary `suchThat` isIdentifier)
