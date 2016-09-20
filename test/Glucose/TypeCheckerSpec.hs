module Glucose.TypeCheckerSpec (spec) where

import Test.Prelude

import Glucose.AST as AST
import Glucose.IR as IR
import Glucose.Identifier
import Glucose.Error
import Glucose.Test.AST as AST
import Glucose.Test.Error
import Glucose.Test.IR as IR
import qualified Glucose.TypeChecker as TC

typeCheck :: AST.Module -> Either CompileError IR.Module
typeCheck = TC.typeCheck

spec :: Spec
spec = describe "typeCheck" $ do
  it "transforms an empty module" $
    typeCheck (AST.Module []) `shouldBe` Right (IR.Module [])
  it "transforms a module with distinct definitions requiring inlining" $
    let input = AST.Module
          [ AST.alias "a" "c"
          , AST.constant "b" $ AST.FloatLiteral 3.21
          , AST.constant "c" $ AST.IntegerLiteral 123
          , AST.alias "d" "a" ]
        expected = IR.Module
          [ IR.definition "a" $ IR.Reference Global (Identifier "c") Integer
          , IR.constant "b" $ IR.FloatLiteral 3.21
          , IR.constant "c" $ IR.IntegerLiteral 123
          , IR.definition "d" $ IR.Reference Global (Identifier "a") Integer ]
    in typeCheck input `shouldBe` Right expected
  it "fails a module with duplicate definitions" $
    let input = AST.Module
          [ AST.constant "a" $ AST.IntegerLiteral 123
          , AST.constant "b" $ AST.FloatLiteral 3.21
          , AST.constant "a" $ AST.FloatLiteral 0 ]
    in typeCheck input `shouldErrorWith` compileError "1:1@0" (DuplicateDefinition (Identifier "a") (read "1:1@0"))
  it "fails a module with recursive definitions" $
    let input = AST.Module
          [ AST.alias "a" "c"
          , AST.constant "b" $ AST.IntegerLiteral 66
          , AST.alias "c" "d"
          , AST.alias "d" "a" ]
    in typeCheck input `shouldErrorWith` compileError "1:1@0" (RecursiveDefinition (Identifier "a"))
