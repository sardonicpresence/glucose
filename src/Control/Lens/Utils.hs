module Control.Lens.Utils where

import Control.Lens
import Control.Monad
import Control.Monad.State.Class

type Recursion s = forall m. Monad m => LensLike m s s s s

modifyM :: MonadState a m => (a -> m a) -> m ()
modifyM f = put =<< f =<< get

modifyingM :: MonadState a m => (t -> a -> m a) -> t -> m ()
modifyingM l f = modifyM $ l f

recursing :: Traversal' s s -> Recursion s
recursing t f = f <=< t (recursing t f)
