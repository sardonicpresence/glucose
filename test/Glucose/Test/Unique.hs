module Glucose.Test.Unique ( module Glucose.Unique ) where

import Glucose.Unique

import Test.QuickCheck.Arbitrary

instance Arbitrary Unique where
  arbitrary = toEnum <$> arbitrary
