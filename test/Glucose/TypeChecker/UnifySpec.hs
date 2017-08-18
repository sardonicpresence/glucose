module Glucose.TypeChecker.UnifySpec (spec) where

import Test.Prelude

import Control.Comonad.Identity
import Control.Lens
import Control.Lens.Utils
import Data.List
import Glucose.IR
import Glucose.Test.IR
-- import Glucose.TypeChecker.TypeCheckError
import Glucose.TypeChecker.Unify

spec :: Spec
spec = describe "unify" $ do
  it "unifies equal tpes unchanged" $ property $ \t ->
    (t, t) `unifiesTo` t
  it "fails to unify two distinct tpes" $ property $ \(Monomorphic a) (Monomorphic b) -> a /= b ==>
    failsToUnify (a, b)
  it "unifies two free tpe variables" $ property $ \a b ->
    (Free a, Free b) `unifiesTo` Free a .||.
    (Free a, Free b) `unifiesTo` Free b
  it "replaces a free tpe variable with a bound tpe" $ property $ \t -> isBound t ==>
    (t, Free "a") `unifyTo` (t, box t) .&&.
    (Free "a", t) `unifyTo` (box t, t)
  it "unifies two bound tpe variables" $ property $ \a b ->
    (Bound (Polymorphic a), Bound (Polymorphic b)) `unifiesTo` Bound (Polymorphic a) .||.
    (Bound (Polymorphic a), Bound (Polymorphic b)) `unifiesTo` Bound (Polymorphic b)
  it "replaces a bound tpe variable with a structured tpe" $ property $ \t -> hasStructure t ==>
    (t, Bound (Polymorphic "a")) `unifyTo` (t, box t) .&&.
    (Bound (Polymorphic "a"), t) `unifyTo` (box t, t)
  it "replaces a polymorphic function with a concrete one" $ property $ \(Monomorphic s) (Monomorphic t) ->
    let a = Free "a"; b = Free "b" in
    (function s t, function a b) `unifyTo` (function s t, function (box s) (box t)) .&&.
    (function s b, function a t) `unifyTo` (function s (box t), function (box s) t) .&&.
    (function a t, function s b) `unifyTo` (function (box s) t, function s (box t)) .&&.
    (function a b, function s t) `unifyTo` (function (box s) (box t), function s t)
  it "correctly unifies nested functions" $ property $ \(Monomorphic q) (Monomorphic r) (Monomorphic s) (Monomorphic t) ->
    let [a, b, c, d] = map Free ["a", "b", "c", "d"] in
    (function (function a q) (function (function r r) b), function (function s c) (function d t)) `unifyTo`
    (function (function (box s) q) (function (function r r) (box t)), function (function s (box q)) (function (function r r) t))
  it "binds all free tpe variable sites" $ property $ \t -> isBound t ==>
    let [a, b, c, d] = map Free ["a", "b", "c", "d"] in
    (function a a, function t b) `unifyTo` (function (box t) (box t), function t (box t)) .&&.
    (function a a, function b t) `unifyTo` (function (box t) (box t), function (box t) t) .&&.
    (function t b, function a a) `unifyTo` (function t (box t), function (box t) (box t)) .&&.
    (function b t, function a a) `unifyTo` (function (box t) t, function (box t) (box t)) .&&.
    (function (function a b) (function b a), function (function t c) (function d d)) `unifyTo`
    (function (function (box t) (box t)) (function (box t) (box t)), function (function t (box t)) (function (box t) (box t)))
  it "replaces all bound tpe variable sites" $ property $ \(Monomorphic t) ->
    let [a, b, c, d] = map (Bound . Polymorphic) ["a", "b", "c", "d"] in
    (function a a, function t b) `unifyTo` (function (box t) (box t), function t (box t)) .&&.
    (function a a, function b t) `unifyTo` (function (box t) (box t), function (box t) t) .&&.
    (function t b, function a a) `unifyTo` (function t (box t), function (box t) (box t)) .&&.
    (function b t, function a a) `unifyTo` (function (box t) t, function (box t) (box t)) .&&.
    (function (function a b) (function b a), function (function t c) (function d d)) `unifyTo`
    (function (function (box t) (box t)) (function (box t) (box t)), function (function t (box t)) (function (box t) (box t)))
  it "results in two identical types, ignoring boxing, or fails to unify" $ property $ \a b -> disjoint a b ==>
    case unify (Identity a) (Identity b) of
      Left _ -> property True
      Right f -> (f a & recursing types %~ box) === (f b & recursing types %~ box)

unifyTo :: (Type Checking, Type Checking) -> (Type Checking, Type Checking) -> Property
unifyTo (a, b) (a', b') = (unify (Identity a) (Identity b) >>= \f -> pure (f a, f b)) === Right (a', b')

unifiesTo :: (Type Checking, Type Checking) -> Type Checking -> Property
unifiesTo from to = unifyTo from (to, to)

failsToUnify :: (Type Checking, Type Checking) -> Property
failsToUnify (a, b) = case unify (Identity a) (Identity b) of
  Left _ -> property True
  Right f -> counterexample ("unified to " ++ show (f a, f b)) False

disjoint :: Type Checking -> Type Checking -> Bool
disjoint a b =
  null ((a ^.. getting bind) `intersect` (b ^.. getting bind)) &&
  null ((a ^.. getting checked) `intersect` (b ^.. getting checked))

box :: Type Checking -> Type Checking
box = dataType %~ boxed

function :: Type Checking -> Type Checking -> Type Checking
function f a = Bound $ Function UnknownArity f a