module Glucose.Codegen.LLVM.ExecutionSpec (spec) where

import Test.Prelude
import Test.Executable

import Data.Maybe (fromJust)
import Data.Text (unpack)
import Glucose.Compiler
import Glucose.Codegen
import Glucose.Codegen.Target

spec :: Spec
spec = describe "LLVM codegen" $ do
  it "can run a trivial execution test" $
    traverse (testRun 123 . unpack) (compile (codegen $ LLVM target) "main = \\a -> 123") `shouldReturn` Right ()

target = fromJust $ parseTriple "x86_64-pc-mingw32"
