module Glucose.Test.IR where

import Glucose.Format ()
import Glucose.IR
import Glucose.Test.Identifier ()
import Glucose.Test.Unique ()
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

instance Arbitrary (Type Checking) where
  arbitrary = Type <$> frequency [ (1, Free <$> arbitrary), (5, Bound <$> arbitrary) ]

instance Arbitrary ty => Arbitrary (DataType ty) where
  arbitrary = oneof
    [ Unboxed <$> arbitrary
    , Boxed <$> arbitrary
    , ADT <$> arbitrary
    , Polymorphic <$> arbitrary
    , Function UnknownArity <$> arbitrary <*> arbitrary
    ]

instance Arbitrary Primitive where
  arbitrary = oneof [ pure Integer, pure Float ]

newtype Monomorphic = Monomorphic { monomorphic :: Type Checking }

instance Show Monomorphic where
  show (Monomorphic ty) = show ty

instance Arbitrary Monomorphic where
  arbitrary = Monomorphic . Type . Bound <$> oneof
    [ Unboxed <$> arbitrary
    , Boxed <$> arbitrary
    , ADT <$> arbitrary
    , Function UnknownArity <$> fmap monomorphic arbitrary <*> fmap monomorphic arbitrary
    ]

isFree :: Type Checking -> Bool
isFree FreeType{} = True
isFree _ = False

isBound :: Type Checking -> Bool
isBound BoundType{} = True
isBound _ = False

hasStructure :: Type Checking -> Bool
hasStructure FreeType{} = False
hasStructure (BoundType Polymorphic{}) = False
hasStructure _ = True
