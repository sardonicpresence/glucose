module Test.Prelude
(
  module Test.Hspec,
  module Test.QuickCheck,
  module Test.Utils
) where

import Test.Hspec hiding (shouldBe)
import Test.QuickCheck
import Test.Utils
