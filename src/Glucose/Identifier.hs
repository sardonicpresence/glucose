module Glucose.Identifier where

import Control.Comonad
import Control.Monad
import Data.Map as Map
import Data.Map.Utils
import Data.String
import Data.Text
import Glucose.Source

newtype Identifier = Identifier { identify :: Text } deriving (Eq, Ord)

instance Show Identifier where
  show (Identifier n) = unpack n

instance IsString Identifier where
  fromString = Identifier . pack

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
