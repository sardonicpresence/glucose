{-# LANGUAGE FunctionalDependencies #-}
module Glucose.Identifier where

import Control.Comonad
import Control.Monad
import Control.Monad.Identity (Identity)
import Data.Map as Map
import Data.Map.Utils
import Data.String
import Data.Text

newtype Identifier = Identifier Text deriving (Eq, Ord)

instance Show Identifier where
  show (Identifier n) = unpack n

instance IsString Identifier where
  fromString = Identifier . pack

class Bound f a | a -> f where
  identifier :: a -> f Identifier

instance Bound Identity Identifier where
  identifier = pure

instance Bound f (f Identifier) where
  identifier = id

identify :: (Comonad f, Bound f a) => a -> Identifier
identify = extract . identifier

bindings :: Monad m => (a -> a -> m (Map Identifier a)) -> (a -> Identifier) -> [a] -> m (Map Identifier a)
bindings onDuplicate identify defs = go defs Map.empty where
  go [] = pure
  go (def:defs) = go defs <=< insertOr (identify def) def (onDuplicate def)
