module Glucose.Identifier where

import Control.Monad
import Data.Map as Map
import Data.Map.Utils
import Data.Text
import Glucose.Lexer.Char
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

newtype Identifier = Identifier { identify :: Text } deriving (Eq, Ord)

instance Show Identifier where
  show (Identifier n) = unpack n

instance Arbitrary Identifier where
  arbitrary = fmap (Identifier . pack) . (:)
    <$> arbitrary `suchThat` (\c -> isIdentifier c && not (isDigit c))
    <*> listOf (arbitrary `suchThat` isIdentifier)

class Bound a where
  identifier :: a -> Identifier

instance Bound Identifier where
  identifier = id

bindings :: (Monad m, Bound a) => (a -> a -> m (Map Identifier a)) -> [a] -> m (Map Identifier a)
bindings onDuplicate defs = go defs Map.empty where
  go [] = pure
  go (def:defs) = go defs <=< insertOr (identifier def) def (onDuplicate def)
