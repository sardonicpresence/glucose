module Glucose.Codegen.LLVMSpec (spec) where

import Test.Prelude

import Glucose.IR as IR
import Glucose.Codegen.LLVM
import Glucose.Test.IR
import LLVM.AST as LLVM

spec :: Spec
spec = describe "LLVM codegen" $ do
  it "compiles an empty module" $
    codegen (IR.Module []) `shouldBe` LLVM.Module []
  it "compiles global numeric constant definitions correctly" $
    codegen (IR.Module [constant "a" $ IR.IntegerLiteral 123,
                        constant "b" $ IR.FloatLiteral 3.21]) `shouldBe`
      LLVM.Module [LLVM.VariableDefinition (LLVM.Name "a") (LLVM.Literal (LLVM.I32 123)),
                   LLVM.VariableDefinition (LLVM.Name "b") (LLVM.Literal (LLVM.F64 3.21))]
  it "compiles global aliases correctly" $
    codegen (IR.Module [alias "a" "b" Integer]) `shouldBe`
      LLVM.Module [LLVM.Alias (mkName "a") (mkName "b") LLVM.TI32]
  it "mangles global identifiers" $
    codegen (IR.Module [constant "-._$\x05d0\&azAZ09\x5d5_" $ IR.IntegerLiteral 0]) `shouldBe`
      LLVM.Module [LLVM.VariableDefinition (LLVM.Name "-._$24$$5d0$azAZ09$5d5$_") (LLVM.Literal (LLVM.I32 0))]
