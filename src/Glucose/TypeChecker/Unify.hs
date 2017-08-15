module Glucose.TypeChecker.Unify (unify) where

import Control.Comonad
import Control.Monad.Except
import Glucose.IR
import Glucose.TypeChecker.TypeCheckError

unify :: (MonadError (TypeCheckError f) m, Comonad f)
 => f (Type Checking) -> f (Type Checking) -> m (Type Checking -> Type Checking)
unify ty1 ty2 = go (extract ty1) (extract ty2) where
  go a b@Free{} = pure $ replace b a
  go a@Free{} b = pure $ replace a b
  go a b@(Bound Polymorphic{}) = pure $ replace b a
  go a@(Bound Polymorphic{}) b = pure $ replace a b
  go (Bound (Function _ f a)) (Bound (Function _ g b)) = do
    j <- unify (f <$ ty1) (g <$ ty2)
    k <- unify (j a <$ ty1) (j b <$ ty2)
    pure $ k . j
  go a b | a /= b = throwError $ TypeMismatch ty1 ty2
  go _ _ = pure id

replace :: Type Checking -> Type Checking -> Type Checking -> Type Checking
replace from to a = if a == from then to else a
