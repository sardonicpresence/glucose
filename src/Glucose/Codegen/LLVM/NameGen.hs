module Glucose.Codegen.LLVM.NameGen where

import Control.Arrow ((&&&))
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import LLVM.Name

newtype NameGenT m a = NameGenT (ReaderT Name (StateT Int m) a)
  deriving (Functor, Applicative, Monad)

type NameGen = NameGenT Identity

instance MonadWriter w m => MonadWriter w (NameGenT m) where
  tell = NameGenT . tell
  listen (NameGenT r) = NameGenT $ listen r
  pass (NameGenT r) = NameGenT $ pass r

instance MonadTrans NameGenT where
  lift = NameGenT . lift . lift

withNewScope :: Monad m => Name -> NameGenT m a -> m a
withNewScope name (NameGenT a) = evalStateT (runReaderT a name) 1

withName :: Monad m => Name -> (Name -> NameGenT m a) -> m a
withName name f = withNewScope name (f name)

newGlobal :: Monad m => NameGenT m Name
newGlobal = NameGenT . ReaderT $ \name -> state $ deriveName name &&& (+1)
