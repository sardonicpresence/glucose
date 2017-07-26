module Glucose.Identifier where

import Control.Comonad
import Control.Monad
import Data.Map as Map
import Data.Map.Utils
import Data.String
import Data.Text
import Glucose.Lexer.Char
import Glucose.Source
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

newtype Identifier = Identifier { identify :: Text } deriving (Eq, Ord)

instance Show Identifier where
  show (Identifier n) = unpack n

instance IsString Identifier where
  fromString = Identifier . pack

instance Arbitrary Identifier where
  arbitrary = fmap (Identifier . pack) . (:)
    <$> arbitrary `suchThat` (\c -> isIdentifier c && not (isDigit c))
    <*> listOf (arbitrary `suchThat` isIdentifier)

class Bound a where
  identifier :: a -> Identifier

instance Bound Identifier where
  identifier = id

instance Bound a => Bound (FromSource a) where
  identifier = identifier . extract

bindings :: (Monad m, Bound a) => (a -> a -> m (Map Identifier a)) -> [a] -> m (Map Identifier a)
bindings onDuplicate defs = go defs Map.empty where
  go [] = pure
  go (def:defs) = go defs <=< insertOr (identifier def) def (onDuplicate def)
