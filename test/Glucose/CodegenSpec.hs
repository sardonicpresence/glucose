module Glucose.CodegenSpec (spec) where

import Test.Prelude

import Data.Text (Text)
import Glucose.IR as IR
import Glucose.Codegen
import Glucose.Identifier
import LLVM.AST as LLVM

spec :: Spec
spec = describe "codegen" $ do
  it "compiles an empty module" $
    codegen (IR.Module []) `shouldBe` LLVM.Module []
  it "compiles global numeric constant definitions correctly" $
    codegen (IR.Module [constant "a" $ IR.IntegerLiteral 123,
                        constant "b" $ IR.FloatLiteral 3.21]) `shouldBe`
      LLVM.Module [LLVM.VariableDefinition (LLVM.Name "a") (LLVM.Literal (LLVM.I32 123)),
                   LLVM.VariableDefinition (LLVM.Name "b") (LLVM.Literal (LLVM.F64 3.21))]
  it "mangles global identifiers" $
    codegen (IR.Module [constant "-._$\x05d0\&azAZ09\x5d5_" $ IR.IntegerLiteral 0]) `shouldBe`
      LLVM.Module [LLVM.VariableDefinition (LLVM.Name "-._$24$$5d0$azAZ09$5d5$_") (LLVM.Literal (LLVM.I32 0))]

constant :: Text -> IR.Literal -> IR.Definition
constant name lit = IR.Definition (Identifier name) (IR.Literal lit)
