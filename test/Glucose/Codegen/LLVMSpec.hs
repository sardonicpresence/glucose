module Glucose.Codegen.LLVMSpec (spec) where

import Test.Prelude

import Glucose.IR as IR
import Glucose.Codegen.LLVM
import Glucose.Test.IR.Checked
import LLVM.AST as LLVM

spec :: Spec
spec = describe "LLVM codegen" $ do
  it "compiles an empty module" $
    codegen (IR.Module []) `shouldBe` LLVM.Module []
  it "compiles global numeric constant definitions correctly" $
    codegen (IR.Module [constantAnywhere "a" $ IR.IntegerLiteral 123,
                        constantAnywhere "b" $ IR.FloatLiteral 3.21]) `shouldBe`
      LLVM.Module [LLVM.VariableDefinition (LLVM.Name "a") (LLVM.Literal (LLVM.I32 123)),
                   LLVM.VariableDefinition (LLVM.Name "b") (LLVM.Literal (LLVM.F64 3.21))]
  it "compiles global aliases correctly" $
    codegen (IR.Module [aliasAnywhere "a" "b" Integer]) `shouldBe`
      LLVM.Module [LLVM.Alias (mkName "a") (mkName "b") LLVM.TI32]
  it "compiles enum constructors correctly" $
    codegen (IR.Module [constructorAnywhere "test" "a" 0, constructorAnywhere "test" "B" 1]) `shouldBe`
      LLVM.Module [LLVM.VariableDefinition (LLVM.Name "a") (LLVM.Literal (LLVM.I32 0)),
                   LLVM.VariableDefinition (LLVM.Name "B") (LLVM.Literal (LLVM.I32 1))]
  it "mangles global identifiers" $
    codegen (IR.Module [constantAnywhere "-._$\x05d0\&azAZ09\x5d5_" $ IR.IntegerLiteral 0]) `shouldBe`
      LLVM.Module [LLVM.VariableDefinition (LLVM.Name "-._$24$$5d0$azAZ09$5d5$_") (LLVM.Literal (LLVM.I32 0))]
