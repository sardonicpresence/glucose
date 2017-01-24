module Glucose.Codegen.LLVMSpec (spec) where

import Test.Prelude

import Glucose.IR as IR
import Glucose.Codegen.LLVM
import Glucose.Test.IR
import LLVM.AST as LLVM
import LLVM.DSL
import LLVM.Name

spec :: Spec
spec = describe "LLVM codegen" $ do
  it "compiles an empty module" $
    codegen (IR.Module []) `shouldBe` LLVM.Module win64 []
  it "compiles global numeric constant definitions correctly" $
    codegenDefinitions [constant "a" $ IR.IntegerLiteral 123,
                        constant "b" $ IR.FloatLiteral 3.21] `shouldBe`
      [LLVM.VariableDefinition (Name "a") LLVM.External (i32 123),
       LLVM.VariableDefinition (Name "b") LLVM.External (f64 3.21)]
  it "compiles global aliases correctly" $
    codegenDefinitions [alias "a" "b" Integer] `shouldBe`
      [LLVM.Alias (Name "a") (Name "b") (LLVM.I 32)]
  it "mangles global identifiers" $
    codegenDefinitions [constant "-._$\x05d0\&azAZ09\x5d5_" $ IR.IntegerLiteral 0] `shouldBe`
      [LLVM.VariableDefinition (Name "-._$24$$5d0$azAZ09$5d5$_") LLVM.External (i32 0)]
