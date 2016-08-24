module Glucose.CompilerSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Text.Lazy
import Glucose.Compiler

spec :: Spec
spec = describe "compile" $ do
  it "compiles valid white-space to an empty module" $
    unpack (compile (pack " \r\n \f\xa0\x85 ")) `shouldBe` ""
