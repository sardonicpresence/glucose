module Data.Functor.With where

import Control.Comonad

{-| The pair functor with a Monad instance. -}
data With a b = With a b

getWith (With a _) = a

instance Functor (With a) where
  fmap f (With a b) = With a (f b)

instance Monoid a => Applicative (With a) where
  pure = With mempty
  With a f <*> With b x = With (mappend a b) (f x)

instance Monoid a => Monad (With a) where
  With a b >>= f = case f b of With c d -> With (mappend a c) d

instance Comonad (With a) where
  extract (With _ b) = b
  duplicate (With a b) = With a (With a b)
