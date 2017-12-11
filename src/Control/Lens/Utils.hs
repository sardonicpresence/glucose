module Control.Lens.Utils where

import Control.Lens
import Control.Monad
import Control.Monad.State.Class
import Data.Functor.With
import Data.Monoid

infixl 8 ^??

type Recursion s = forall m. Monad m => LensLike m s s s s

modifies :: MonadState s m => (a -> s -> s) -> a -> m a
modifies f a = a <$ modify (f a)

modifyM :: MonadState a m => (a -> m a) -> m ()
modifyM f = put =<< f =<< get

modifyingM :: MonadState a m => (t -> a -> m a) -> t -> m ()
modifyingM l f = modifyM $ l f

recursing :: Traversal' s s -> Recursion s
recursing t f = f <=< t (recursing t f)

traverseMapOf :: LensLike' (With r) s s -> (s -> r) -> s -> r
traverseMapOf l f = getWith . l (\a -> With (f a) a)

(^??) :: s -> LensLike' (With (First s)) s s -> Maybe s
s ^?? l = getFirst $ traverseMapOf l (First . Just) s
