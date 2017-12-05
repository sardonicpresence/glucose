module Glucose.Test.IR where

import Glucose.Format ()
import Glucose.IR
import Glucose.Test.Identifier ()
import Glucose.Test.Unique ()
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

instance Arbitrary (Type Checking) where
  arbitrary = Type <$> frequency [ (1, Any <$> arbitrary), (1, Free <$> arbitrary), (5, Bound <$> arbitrary) ]

instance Arbitrary ty => Arbitrary (DataType ty) where
  arbitrary = oneof
    [ pure Integer
    , pure Float
    , ADT <$> arbitrary
    , Polymorphic <$> arbitrary
    , Constrained <$> oneof
      [ pure Integer
      , pure Float
      , ADT <$> arbitrary
      , Function UnknownArity <$> arbitrary <*> arbitrary
      ]
    , Function UnknownArity <$> arbitrary <*> arbitrary
    ]

newtype Monomorphic = Monomorphic { monomorphic :: Type Checking }

instance Show Monomorphic where
  show (Monomorphic ty) = show ty

instance Arbitrary Monomorphic where
  arbitrary = Monomorphic . Type . Bound <$> arbitraryMonomorphic where
    arbitraryMonomorphic = oneof
      [ pure Integer
      , pure Float
      , ADT <$> arbitrary
      , Constrained <$> oneof
        [ pure Integer
        , pure Float
        , ADT <$> arbitrary
        , Function UnknownArity <$> fmap monomorphic arbitrary <*> fmap monomorphic arbitrary
        ]
      , Function UnknownArity <$> fmap monomorphic arbitrary <*> fmap monomorphic arbitrary
      ]

isFree :: Type Checking -> Bool
isFree FreeType{} = True
isFree _ = False

isBound :: Type Checking -> Bool
isBound BoundType{} = True
isBound _ = False

hasStructure :: Type Checking -> Bool
hasStructure AnyType{} = False
hasStructure FreeType{} = False
hasStructure (BoundType Polymorphic{}) = False
hasStructure _ = True
