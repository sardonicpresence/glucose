module Glucose.TypeChecker.Unify (unify) where

import Control.Comonad
import Control.Lens
import Control.Lens.Utils
import Control.Monad.Except
import Glucose.IR
import Glucose.TypeChecker.TypeCheckError

unify :: (MonadError (TypeCheckError f) m, Comonad f)
 => f (Type Checking) -> f (Type Checking) -> m (Type Checking -> Type Checking)
unify ty1 ty2 = go (extract ty1) (extract ty2) where
  go a b@FreeType{} = pure $ replace b (a & dataType %~ boxed)
  go a@FreeType{} b = pure $ replace a (b & dataType %~ boxed)
  go a b@(BoundType Polymorphic{}) = pure $ replace b (a & dataType %~ unboxed)
  go a@(BoundType Polymorphic{}) b = pure $ replace a (b & dataType %~ unboxed)
  go (BoundType (Function _ a b)) (BoundType (Function _ c d)) = do
    f <- unify (a <$ ty1) (c <$ ty2)
    g <- unify (f b <$ ty1) (f d <$ ty2)
    pure $ g . f
  go a b | a /= b = throwError $ TypeMismatch ty1 ty2
  go _ _ = pure id

replace :: Type Checking -> Type Checking -> Type Checking -> Type Checking
replace from to = recursing types %~ \a -> if a == from then to else a
