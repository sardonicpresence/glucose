module Glucose.TypeChecker.Unify (unify) where

import Control.Comonad
import Control.Lens
import Control.Lens.Utils
import Control.Monad.Except
import Control.Monad.State (evalState, state)
import Data.Maybe (isJust)
import Glucose.IR
import Glucose.TypeChecker.TypeCheckError
import Glucose.VarGen

unify :: (Comonad f, Traversable f, MonadError (TypeCheckError f) m)
 => Type Checking -> f (Type Checking) -> m (Type Checking -> Type Checking)
unify expected actual = either (throwTypeCheckError expected actual) pure $ go expected (extract actual) where
  go a b@(Type Any{}) = replace b a
  go a@(Type Any{}) b = replace a b
  go a b@FreeType{} = replace b (a & dataType %~ boxed)
  go a@FreeType{} b = replace a (b & dataType %~ boxed)
  go a b@(BoundType Polymorphic{}) = replace b (a & dataType %~ unboxed)
  go a@(BoundType Polymorphic{}) b = replace a (b & dataType %~ unboxed)
  go (previews dataType unboxed -> Just (Function _ a b)) (previews dataType unboxed -> Just (Function _ c d)) = do
    f <- go a c
    g <- go (f b) (f d)
    Right $ g . f
  go a b | a /= b = Left TypeMismatch
  go _ _ = Right id

replace :: Type Checking -> Type Checking -> Either (Type Checked -> f (Type Checked) -> TypeCheckError f) (Type Checking -> Type Checking)
replace from to | from == to = Right id
replace from to | isJust (to ^?? recursing types . filtered (== from)) = Left InfiniteType
replace from to = Right $ recursing types %~ \a -> if a == from then to else a

throwTypeCheckError :: (Functor f, MonadError (TypeCheckError f) m)
 => Type Checking -> f (Type Checking) -> (Type Checked -> f (Type Checked) -> TypeCheckError f) -> m a
throwTypeCheckError a b err = throwError $ err (remapTypes a) (remapTypes <$> b)
  where remapTypes = flip evalState mkVarGen . remap bind (state genVar)
