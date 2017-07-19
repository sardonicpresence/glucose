module Control.Monad.Throw where

import qualified Control.Monad.Except as Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State.Lazy as Lazy
import Control.Monad.State.Strict as Strict
import Control.Monad.RWS

class Monad m => MonadThrow e m where
  throwError :: e -> m a

instance MonadThrow e (Either e) where
  throwError = Left

instance Monad m => MonadThrow e (Except.ExceptT e m) where
  throwError = Except.throwError

throwErrorMonadTrans :: (MonadTrans t, MonadThrow e m) => e -> t m a
throwErrorMonadTrans = lift . throwError

instance MonadThrow e m => MonadThrow e (ReaderT s m) where
  throwError = throwErrorMonadTrans

instance (Monoid w, MonadThrow e m) => MonadThrow e (WriterT w m) where
  throwError = throwErrorMonadTrans

instance MonadThrow e m => MonadThrow e (Lazy.StateT s m) where
  throwError = throwErrorMonadTrans

instance MonadThrow e m => MonadThrow e (Strict.StateT s m) where
  throwError = throwErrorMonadTrans

instance (Monoid w, MonadThrow e m) => MonadThrow e (RWST r w s m) where
  throwError = throwErrorMonadTrans

class MonadThrow e m => MonadCatch e m where
  catchError :: (e -> m a) -> m a -> m a

instance MonadCatch e (Either e) where
  catchError _ (Right a) = Right a
  catchError f (Left e) = f e

instance MonadThrow e m => MonadCatch e (Except.ExceptT e m) where
  catchError = flip Except.catchError
