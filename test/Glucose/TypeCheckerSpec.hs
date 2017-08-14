module Glucose.TypeCheckerSpec (spec) where

import Test.Prelude

import Control.Monad
import Data.Text (Text)
import Glucose.Identifier
import Glucose.IR
import Glucose.Error
import Glucose.Compiler (tokenise, parse, desugar)
import qualified Glucose.Compiler as TC (typeCheck)
import Glucose.Source
import Glucose.Test.IR.Checked
import Glucose.TypeChecker.TypeCheckError

spec :: Spec
spec = describe "typeCheck" $ do
  it "passes an empty module" $
    typeCheck "" `shouldBe` Right (Module [])
  it "type-checks a module with distinct definitions, aliases and enums" $
    let input = "a=c\nb=3.21\nc=123\ntype it=This|that\nd=a"
        expected = Module
          [ alias ("a" `at` "1:1@0-1:1@0") ("c" `at` "1:3@2-1:3@2") (Unboxed Integer)
          , constant ("b" `at` "2:1@4-2:1@4") (FloatLiteral 3.21 `at` "2:3@6-2:6@9")
          , constant ("c" `at` "3:1@11-3:1@11") (IntegerLiteral 123 `at` "3:3@13-3:5@15")
          , constructor ("it" `at` "4:6@22-4:7@23") ("This" `at` "4:9@25-4:12@28") 0
          , constructor ("it" `at` "4:6@22-4:7@23") ("that" `at` "4:14@30-4:17@33") 1
          , alias ("d" `at` "5:1@35-5:1@35") ("a" `at` "5:3@37-5:3@37") (Unboxed Integer) ]
    in typeCheck input `shouldBe` Right expected
  it "fails a module with duplicate variable definitions" $
    let input = "a=1\nb=2\na=3"
    in typeCheck input `shouldErrorWith` duplicateDefinition "3:1@8-3:1@8" "a" "1:1@0-1:1@0"
  it "fails a module with duplicate constructor definitions" $
    let input = "type A=a\ntype B=a|b"
    in typeCheck input `shouldErrorWith` duplicateDefinition "2:8@16-2:8@16" "a" "1:8@7-1:8@7"
  it "fails a module with conflicting variable and constructor definitions" $
    let input = "type A=a|b\nb=1"
    in typeCheck input `shouldErrorWith` duplicateDefinition "2:1@11-2:1@11" "b" "1:10@9-1:10@9"
  it "fails a module with duplicate type names" $
    let input = "type A=a\ntype A=b"
    in typeCheck input `shouldErrorWith` duplicateDefinition "2:6@14-2:6@14" "A" "1:6@5-1:6@5"
  it "fails a module with recursive definitions" $
    let input = "a=c\nb=66\nc=d\nd=a"
    in typeCheck input `shouldErrorWith` recursiveDefinition "1:1@0-1:1@0" "a"

typeCheck :: Text -> Either CompileError (Module Checked FromSource)
typeCheck = TC.typeCheck <=< desugar <=< uncurry parse <=< tokenise

duplicateDefinition :: String -> Text -> String -> CompileError
duplicateDefinition loc name prev = TypeCheckError $ DuplicateDefinition (Identifier name `at` loc) (() `at` prev)

recursiveDefinition :: String -> Text -> CompileError
recursiveDefinition loc name = TypeCheckError $ RecursiveDefinition $ Identifier name `at` loc
