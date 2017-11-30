module Glucose.Codegen.LLVM.ExecutionSpec (spec) where

import Test.Prelude
import Test.Executable

import Data.Maybe (fromJust)
import Data.Text (Text, pack, unpack)
import Glucose.Compiler
import Glucose.Codegen
import Glucose.Codegen.Target

spec :: Spec
spec = describe "LLVM codegen" $
  it "can generate an executable example" $
    testExitCode 3 . pack $ unlines
      [ "type Colour = Red | Blue | Green"
      , "red = Red"
      , "three = \\a -> 3"
      , "id = \\a -> a"
      , "test = \\a -> id three red" ]

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
