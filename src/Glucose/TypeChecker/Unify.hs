module Glucose.TypeChecker.Unify (unify) where

import Control.Comonad
import Control.Lens
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
  go (Bound (Function _ a b)) (Bound (Function _ c d)) = do
    f <- unify (a <$ ty1) (c <$ ty2)
    g <- unify (f b <$ ty1) (f d <$ ty2)
    pure $ g . f
  go a b | a /= b = throwError $ TypeMismatch ty1 ty2
  go _ _ = pure id

typeVariables :: Traversal' (Type Checking) (Type Checking)
typeVariables f ty@Free{} = f ty
typeVariables f ty@(Bound Polymorphic{}) = f ty
typeVariables f (Bound (Function arity a b)) = Bound <$> (Function arity <$> typeVariables f a <*> typeVariables f b)
typeVariables _ ty = pure ty

replace :: Type Checking -> Type Checking -> Type Checking -> Type Checking
replace from to = typeVariables %~ \a -> if a == from then to & bound %~ boxed else a
