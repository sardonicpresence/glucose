module Glucose.TypeCheckerSpec (spec) where

import Test.Prelude

import Data.Text (Text)
import Glucose.AST as AST
import Glucose.Lexer.Location
import Glucose.TypeChecker

spec :: Spec
spec = describe "typeCheck" $ do
  it "passes an empty module" $
    typeCheck (AST.Module []) `shouldBe` Right (AST.Module [])
  it "passes a module with distinct definitions" $
    let input = AST.Module
          [ constant "a" $ AST.IntegerLiteral 123
          , constant "b" $ AST.FloatLiteral 3.21 ]
    in typeCheck input `shouldBe` Right input
  it "fails a module with duplicate definitions" $
    let input = AST.Module
          [ constant "a" $ AST.IntegerLiteral 123
          , constant "b" $ AST.FloatLiteral 3.21
          , constant "a" $ AST.FloatLiteral 0 ]
    in typeCheck input `shouldErrorContaining` "duplicate definition of \"a\""

constant :: Text -> AST.Literal -> AST.Definition
constant name lit = AST.Definition (AST.Identifier name) lit beginning
