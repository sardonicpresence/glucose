module Glucose.Codegen.ExecutionSpec (spec) where

import Test.Prelude
import Test.Executable

import Data.Maybe (fromJust)
import Data.Text (Text, pack, unpack)
import Glucose.Compiler
import Glucose.Codegen
import Glucose.Codegen.Target

spec :: Spec
spec = do
  describe "LLVM codegen" $
    it "can generate an executable example" $
      testExitCode (LLVM target) llvmHarness 3 source
  describe "JavaScript codegen" $
    it "can generate an executable example" $
      testExitCode JavaScript jsHarness 3 source

source = pack $ unlines
  [ "type Colour = Red | Blue | Green"
  , "red = Red"
  , "three = \\a -> 3"
  , "id = \\a -> a"
  , "const = \\a -> id"
  , "test = \\a -> const 2 three red" ]

testExitCode :: CompilerOutput -> String -> Int -> Text -> Expectation
testExitCode outputType harness exitcode source =
  let llvmIR = compile (codegen outputType) source in
  traverse (testRun (executable outputType) exitcode . (++ harness) . unpack) llvmIR `shouldReturn` Right ()

executable :: CompilerOutput -> String
executable JavaScript = "node"
executable LLVM{} = "lli"
executable outputType = error $ "Output type is not executable: " ++ show outputType

llvmHarness = unlines
  [ "define i32 @main() {"
  , "  %1 = call fastcc i32 @test(%$box zeroinitializer)"
  , "  ret i32 %1"
  , "}"]

jsHarness = "process.exit(test())"

target = fromJust $ parseTriple "x86_64-pc-mingw32"
