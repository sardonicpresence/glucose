module Glucose.Test.IR where

import Glucose.IR
import Glucose.Test.Identifier ()
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

isBound :: Type Checking -> Bool
isBound (Type Bound{}) = True
isBound _ = False

hasStructure :: Type Checking -> Bool
hasStructure (Type Free{}) = False
hasStructure (Type (Bound Polymorphic{})) = False
hasStructure _ = True
