module Glucose.Codegen.LLVM.ExecutionSpec (spec) where

import Test.Prelude
import Test.Executable

import Data.Maybe (fromJust)
import Data.Text (Text, unpack)
import Glucose.Compiler
import Glucose.Codegen
import Glucose.Codegen.Target

spec :: Spec
spec = describe "LLVM codegen" $ do
  it "can run a trivial execution test" $
    testExitCode 123 "test = \\a -> 123"

testExitCode :: Int -> Text -> Expectation
testExitCode exitcode source =
  let llvmIR = compile (codegen $ LLVM target) source in
  traverse (testRun exitcode . (++ harness) . unpack) llvmIR `shouldReturn` Right ()

harness = unlines
  [ "define i32 @main() {"
  , "  %1 = call fastcc i32 @test(%$box zeroinitializer)"
  , "  ret i32 %1"
  , "}"]

target = fromJust $ parseTriple "x86_64-pc-mingw32"
