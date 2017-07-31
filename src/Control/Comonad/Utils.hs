module Control.Comonad.Utils ( module Control.Comonad, mapC ) where

import Control.Comonad

mapC :: (Functor f, Comonad m) => (a -> f b) -> m a -> f (m b)
mapC f a = (a $>) <$> f (extract a)
