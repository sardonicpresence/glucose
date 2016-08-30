module Glucose.CodegenSpec (spec) where

import Test.Prelude

import Data.Text (Text)
import Glucose.AST as AST
import Glucose.Codegen
import Glucose.Lexer.Location
import LLVM.AST as LLVM

spec :: Spec
spec = describe "codegen" $ do
  it "compiles an empty module" $
    codegen (AST.Module []) `shouldBe` LLVM.Module []
  it "compiles global numeric constant definitions correctly" $
    codegen (AST.Module [constant "a" $ AST.IntegerLiteral 123,
                         constant "b" $ AST.FloatLiteral 3.21]) `shouldBe`
      LLVM.Module [LLVM.VariableDefinition (LLVM.Name "a") (LLVM.I32 123),
                   LLVM.VariableDefinition (LLVM.Name "b") (LLVM.F64 3.21)]
  it "mangles global identifiers" $
    codegen (AST.Module [constant "-._$\x05d0\&azAZ09\x5d5_" $ AST.IntegerLiteral 0]) `shouldBe`
      LLVM.Module [LLVM.VariableDefinition (LLVM.Name "-._$24$$5d0$azAZ09$5d5$_") (LLVM.I32 0)]

constant :: Text -> AST.Literal -> AST.Definition
constant name lit = AST.Definition (AST.Identifier name) lit beginning
