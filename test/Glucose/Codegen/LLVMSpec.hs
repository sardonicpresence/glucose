module Glucose.Codegen.LLVMSpec (spec) where

import Test.Prelude

import Control.Comonad
import Control.Comonad.Identity
import Data.Maybe (fromJust)
import Glucose.IR.Checked as IR
import Glucose.Codegen.LLVM as LLVM hiding (codegenDefinitions)
import qualified Glucose.Codegen.LLVM as LLVM (codegenDefinitions)
import Glucose.Codegen.Target
import Glucose.Test.IR.Checked
import LLVM.AST as LLVM
import LLVM.DSL
import LLVM.Name

spec :: Spec
spec = describe "LLVM codegen" $ do
  it "compiles an empty module" $
    codegenModule target (IR.Module [] :: IR.Module Identity) `shouldBe` LLVM.Module (llvmTarget target) []
  it "compiles global numeric constant definitions correctly" $
    codegenDefinitions [constant' "a" $ IR.IntegerLiteral 123,
                        constant' "b" $ IR.FloatLiteral 3.21] `shouldBe`
      [LLVM.VariableDefinition (Name "a") LLVM.External Unnamed (i32 123) alignment,
       LLVM.VariableDefinition (Name "b") LLVM.External Unnamed (f64 3.21) alignment]
  it "compiles global aliases correctly" $
    codegenDefinitions [alias' "a" "b" $ Unboxed Integer] `shouldBe`
      [LLVM.Alias (Name "a") LLVM.External Unnamed (GlobalReference (Name "b") (LLVM.I 32)) (LLVM.I 32)]
  it "compiles enum constructors correctly" $
    codegenDefinitions [constructor' "test" "a" 0, constructor' "test" "B" 1] `shouldBe`
      [LLVM.VariableDefinition (Name "a") LLVM.External Unnamed (i32 0) alignment,
       LLVM.VariableDefinition (Name "B") LLVM.External Unnamed (i32 1) alignment]
  it "mangles global identifiers" $
    codegenDefinitions [constant' "-._$\x05d0\&azAZ09\x5d5_" $ IR.IntegerLiteral 0] `shouldBe`
      [LLVM.VariableDefinition (Name "-._$24$$5d0$azAZ09$5d5$_") LLVM.External Unnamed (i32 0) alignment]

codegenDefinitions :: [Identity (Definition Identity)] -> [Global]
codegenDefinitions = LLVM.codegenDefinitions . map extract

alignment = Alignment 0
target = fromJust $ parseTriple "x86_64-pc-mingw32"
