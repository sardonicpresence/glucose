module Glucose.Codegen.LLVM.LocalScope where

import Control.Applicative
import Control.Arrow
import Control.Monad.Identity
import Control.Monad.State

newtype LocalScopeT m a = LocalScopeT (StateT Int m a)
  deriving (Functor, Applicative, Alternative, Monad, MonadPlus)

type LocalScope a = LocalScopeT Identity a

withNewScopeT :: Monad m => LocalScopeT m a -> m a
withNewScopeT (LocalScopeT a) = evalStateT a 1

withNewScope :: LocalScope a -> a
withNewScope = runIdentity . withNewScopeT

newLocal :: Monad m => LocalScopeT m Int
newLocal = LocalScopeT . state $ id &&& (+1)
