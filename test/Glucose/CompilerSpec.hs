module Glucose.CompilerSpec (spec) where

import Test.Hspec
import Test.QuickCheck

import Control.Monad
import Data.Text.Lazy
import Glucose.Compiler

shouldErrorContaining :: Either String a -> String -> Expectation
shouldErrorContaining (Right _) _ = expectationFailure "did not fail"
shouldErrorContaining (Left e) s = e `shouldContain` s

spec :: Spec
spec = describe "compile" $ do
  it "compiles white-space to an empty module" $
    unpack <$> compile (pack " \r\n \f\xa0\x85 ") `shouldBe` Right ""
  it "fails on non-white-space" $
    compile (pack " \n  $~") `shouldErrorContaining` "$~"
