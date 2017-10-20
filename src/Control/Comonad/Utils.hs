module Control.Comonad.Utils ( module Control.Comonad, mapC, forget ) where

import Control.Comonad
import Control.Comonad.Identity

mapC :: (Functor f, Comonad m) => (a -> f b) -> m a -> f (m b)
mapC f a = (a $>) <$> f (extract a)

forget :: Comonad f => (a -> b) -> f a -> Identity b
forget f = pure . f . extract
