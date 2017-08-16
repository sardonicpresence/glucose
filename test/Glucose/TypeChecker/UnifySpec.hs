module Glucose.TypeChecker.UnifySpec (spec) where

import Test.Prelude

import Control.Comonad.Identity
import Glucose.IR
import Glucose.Test.IR
-- import Glucose.TypeChecker.TypeCheckError
import Glucose.TypeChecker.Unify

spec :: Spec
spec = describe "unify" $ do
  it "unifies equal types unchanged" $ property $ \ty ->
    (ty, ty) `unifiesTo` ty
  it "unifies two free type variables" $ property $ \a b ->
    (Free a, Free b) `unifiesTo` Free a .||.
    (Free a, Free b) `unifiesTo` Free b
  it "replaces a free type variable with a bound type" $ property $ \ty -> isBound ty ==>
    (ty, Free "a") `unifiesTo` ty .&&.
    (Free "a", ty) `unifiesTo` ty
  it "unifies two bound type variables" $ property $ \a b ->
    (Bound (Polymorphic a), Bound (Polymorphic b)) `unifiesTo` Bound (Polymorphic a) .||.
    (Bound (Polymorphic a), Bound (Polymorphic b)) `unifiesTo` Bound (Polymorphic b)
  it "replaces a bound type variable with a structured type" $ property $ \ty -> hasStructure ty ==>
    (ty, Free "a") `unifiesTo` ty .&&.
    (Free "a", ty) `unifiesTo` ty
  it "fails to unify two distinct types" $ property $ \(Monomorphic a) (Monomorphic b) -> a /= b ==>
    failsToUnify (a, b)
  it "replaces a polymorphic function with a concrete one" $ property $ \(Monomorphic f) (Monomorphic a) ->
    let g = Free "g"; b = Free "b" in
    (function f a, function g b) `unifiesTo` function f a .&&.
    (function f b, function g a) `unifiesTo` function f a .&&.
    (function g a, function f b) `unifiesTo` function f a .&&.
    (function g b, function f a) `unifiesTo` function f a
  it "correctly unifies nested functions" $ property $ \(Monomorphic a) (Monomorphic b) (Monomorphic c) (Monomorphic d) ->
    let w = Free "w"; x = Free "x"; y = Free "y"; z = Free "z" in
    (function (function w a) (function (function b b) x), function (function c y) (function z d)) `unifiesTo`
    function (function c a) (function (function b b) d)
  it "binds all free type variable sites" $ property $ \ty -> isBound ty ==>
    let a = Free "a"; b = Free "b"; c = Free "c"; d = Free "d" in
    (function a a, function ty b) `unifiesTo` function ty ty .&&.
    (function a a, function b ty) `unifiesTo` function ty ty .&&.
    (function ty b, function a a) `unifiesTo` function ty ty .&&.
    (function b ty, function a a) `unifiesTo` function ty ty .&&.
    (function (function a b) (function b a), function (function ty c) (function d d))  `unifiesTo` function (function ty ty) (function ty ty)
  it "replaces all bound type variable sites" $ property $ \(Monomorphic ty) ->
    let a = Bound (Polymorphic "a"); b = Bound (Polymorphic "b"); c = Bound (Polymorphic "c"); d = Bound (Polymorphic "d") in
    (function a a, function ty b) `unifiesTo` function ty ty .&&.
    (function a a, function b ty) `unifiesTo` function ty ty .&&.
    (function ty b, function a a) `unifiesTo` function ty ty .&&.
    (function b ty, function a a) `unifiesTo` function ty ty .&&.
    (function (function a b) (function b a), function (function ty c) (function d d))  `unifiesTo` function (function ty ty) (function ty ty)
  -- TODO: more failure cases; recursive types

unifiesTo :: (Type Checking, Type Checking) -> Type Checking -> Property
unifiesTo (a, b) c = (unify (Identity a) (Identity b) >>= \f -> pure (f a, f b)) === Right (c, c)

failsToUnify :: (Type Checking, Type Checking) -> Property
failsToUnify (a, b) = case unify (Identity a) (Identity b) of
  Left _ -> property True
  Right f -> counterexample ("unified to " ++ show (f a, f b)) False

function :: Type Checking -> Type Checking -> Type Checking
function f a = Bound $ Function UnknownArity f a
