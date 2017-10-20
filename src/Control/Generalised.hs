module Control.Generalised where

class Generalised t where
  rewrap :: (forall a b. (a -> b) -> f a -> g b) -> t f -> t g
