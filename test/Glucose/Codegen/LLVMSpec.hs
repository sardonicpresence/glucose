module Glucose.Codegen.LLVMSpec (spec) where

import Test.Prelude

import Control.Comonad
import Glucose.IR.Checked as IR
import Glucose.Codegen.LLVM as LLVM hiding (codegenDefinitions)
import qualified Glucose.Codegen.LLVM as LLVM (codegenDefinitions)
import Glucose.Source (FromSource)
import Glucose.Test.IR.Checked
import LLVM.AST as LLVM
import LLVM.DSL
import LLVM.Name

spec :: Spec
spec = describe "LLVM codegen" $ do
  it "compiles an empty module" $
    codegenModule (IR.Module []) `shouldBe` LLVM.Module win64 []
  it "compiles global numeric constant definitions correctly" $
    codegenDefinitions [constantAnywhere "a" $ IR.IntegerLiteral 123,
                        constantAnywhere "b" $ IR.FloatLiteral 3.21] `shouldBe`
      [LLVM.VariableDefinition (Name "a") LLVM.External (i32 123),
       LLVM.VariableDefinition (Name "b") LLVM.External (f64 3.21)]
  it "compiles global aliases correctly" $
    codegenDefinitions [aliasAnywhere "a" "b" Integer] `shouldBe`
      [LLVM.Alias (Name "a") (GlobalReference (Name "b") (LLVM.I 32)) (LLVM.I 32)]
  it "compiles enum constructors correctly" $
    codegenDefinitions [constructorAnywhere "test" "a" 0, constructorAnywhere "test" "B" 1] `shouldBe`
      [LLVM.VariableDefinition (Name "a") LLVM.External (i32 0),
       LLVM.VariableDefinition (Name "B") LLVM.External (i32 1)]
  it "mangles global identifiers" $
    codegenDefinitions [constantAnywhere "-._$\x05d0\&azAZ09\x5d5_" $ IR.IntegerLiteral 0] `shouldBe`
      [LLVM.VariableDefinition (Name "-._$24$$5d0$azAZ09$5d5$_") LLVM.External (i32 0)]

codegenDefinitions :: [FromSource Definition] -> [Global]
codegenDefinitions = LLVM.codegenDefinitions . map extract
