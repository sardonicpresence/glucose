module Glucose.TypeChecker.Unify (unify) where

import Control.Comonad
import Control.Lens
import Control.Lens.Utils
import Control.Monad.Except
import Control.Monad.State (evalState, state)
import Glucose.IR
import Glucose.TypeChecker.TypeCheckError
import Glucose.VarGen

unify :: (Comonad f, Traversable f, MonadError (TypeCheckError f) m)
 => Type Checking -> f (Type Checking) -> m (Type Checking -> Type Checking)
unify expected actual = maybe (throwError $ typeMismatch expected actual) pure $ go expected (extract actual) where
  go a b@(Type Any{}) = pure $ replace b a
  go a@(Type Any{}) b = pure $ replace a b
  go a b@FreeType{} = pure $ replace b (a & dataType %~ boxed)
  go a@FreeType{} b = pure $ replace a (b & dataType %~ boxed)
  go a b@(BoundType Polymorphic{}) = pure $ replace b (a & dataType %~ unboxed)
  go a@(BoundType Polymorphic{}) b = pure $ replace a (b & dataType %~ unboxed)
  go (BoundType (Function _ a b)) (BoundType (Function _ c d)) = do
    f <- go a c
    g <- go (f b) (f d)
    pure $ g . f
  go a b | a /= b = Nothing
  go _ _ = pure id

replace :: Type Checking -> Type Checking -> Type Checking -> Type Checking
replace from to = recursing types %~ \a -> if a == from then to else a

typeMismatch :: Traversable f => Type Checking -> f (Type Checking) -> TypeCheckError f
typeMismatch a b = TypeMismatch (remapTypes a) (remapTypes <$> b)
  where remapTypes = flip evalState mkVarGen . remap bind (state genVar)
