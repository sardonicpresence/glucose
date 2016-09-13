module Glucose.TypeCheckerSpec (spec) where

import Test.Prelude

import Data.Text (Text)
import Glucose.AST as AST
import Glucose.Identifier
import Glucose.IR as IR
import Glucose.Lexer.Location
import Glucose.TypeChecker

spec :: Spec
spec = describe "typeCheck" $ do
  it "transforms an empty module" $
    typeCheck (AST.Module []) `shouldBe` Right (IR.Module [])
  it "transforms a module with distinct definitions requiring inlining" $
    let input = AST.Module
          [ alias "a" "c"
          , constant "b" $ AST.FloatLiteral 3.21
          , constant "c" $ AST.IntegerLiteral 123
          , alias "d" "a" ]
        expected = IR.Module
          [ IR.Definition (Identifier "a") (IR.Literal $ IR.IntegerLiteral 123)
          , IR.Definition (Identifier "b") (IR.Literal $ IR.FloatLiteral 3.21)
          , IR.Definition (Identifier "c") (IR.Literal $ IR.IntegerLiteral 123)
          , IR.Definition (Identifier "d") (IR.Literal $ IR.IntegerLiteral 123) ]
    in typeCheck input `shouldBe` Right expected
  it "fails a module with duplicate definitions" $
    let input = AST.Module
          [ constant "a" $ AST.IntegerLiteral 123
          , constant "b" $ AST.FloatLiteral 3.21
          , constant "a" $ AST.FloatLiteral 0 ]
    in typeCheck input `shouldErrorContaining` "duplicate definition of \"a\""
  it "fails a module with recursive definitions" $
    let input = AST.Module
          [ alias "a" "c"
          , constant "b" $ AST.IntegerLiteral 66
          , alias "c" "d"
          , alias "d" "a" ]
    in typeCheck input `shouldErrorContaining` "recursive definition"

constant :: Text -> AST.Literal -> AST.Definition
constant name lit = AST.Definition (Identifier name) (AST.Literal lit) beginning

alias :: Text -> Text -> AST.Definition
alias to from = AST.Definition (Identifier to) (AST.Variable (Identifier from)) beginning
