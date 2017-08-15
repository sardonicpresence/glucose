module Glucose.TypeChecker.UnifySpec (spec) where

import Test.Prelude

import Control.Comonad.Identity
import Data.Either
import Glucose.IR
import Glucose.Test.IR
import Glucose.TypeChecker.TypeCheckError
import Glucose.TypeChecker.Unify

spec :: Spec
spec = describe "unify" $ do
  it "unifies equal types unchanged" $ property $ \ty ->
    (ty, ty) `unifiesTo` (ty, ty)
  it "unifies two free type variables" $ property $ \a b ->
    (Free a, Free b) `unifiesTo` (Free a, Free a) .||.
    (Free a, Free b) `unifiesTo` (Free b, Free b)
  it "replaces a free type variable with a bound type" $ property $ \ty -> isBound ty ==>
    (ty, Free "_") `unifiesTo` (ty, ty) .&&.
    (Free "_", ty) `unifiesTo` (ty, ty)
  it "unifies two bound type variables" $ property $ \a b ->
    (Bound (Polymorphic a), Bound (Polymorphic b)) `unifiesTo` (Bound (Polymorphic a), Bound (Polymorphic a)) .||.
    (Bound (Polymorphic a), Bound (Polymorphic b)) `unifiesTo` (Bound (Polymorphic b), Bound (Polymorphic b))
  it "replaces a bound type variable with a structured type" $ property $ \ty -> hasStructure ty ==>
    (ty, Free "_") `unifiesTo` (ty, ty) .&&.
    (Free "_", ty) `unifiesTo` (ty, ty)
  it "fails to unify two distinct types" $ property $ \a b -> a /= b && isMonomorphic a && isMonomorphic b ==>
    failsToUnify (a, b)

unifiesTo :: (Type Checking, Type Checking) -> (Type Checking, Type Checking) -> Property
unifiesTo (a, b) (a', b') = (unify (Identity a) (Identity b) >>= \f -> pure (f a, f b)) === Right (a', b')

failsToUnify :: (Type Checking, Type Checking) -> Property
failsToUnify (a, b) = case unify (Identity a) (Identity b) of
  Left _ -> 
