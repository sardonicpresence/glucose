module Control.Monad.Throw where

import qualified Control.Monad.Except as Except
import Control.Monad.Reader

class Monad m => MonadThrow e m where
  throwError :: e -> m a

instance MonadThrow e (Either e) where
  throwError = Left

instance (MonadTrans t, Monad (t m), MonadThrow e m) => MonadThrow e (t m) where
  throwError = lift . throwError

class MonadThrow e m => MonadCatch e m where
  catchError :: (e -> m a) -> m a -> m a

instance MonadCatch e (Either e) where
  catchError _ (Right a) = Right a
  catchError f (Left e) = f e

instance MonadThrow e m => MonadCatch e (Except.ExceptT e m) where
  catchError = flip Except.catchError
