module Control.Lens.Utils where

import Control.Monad.State.Class

modifyM :: MonadState a m => (a -> m a) -> m ()
modifyM f = put =<< f =<< get

modifyingM :: MonadState a m => (t -> a -> m a) -> t -> m ()
modifyingM l f = modifyM $ l f
