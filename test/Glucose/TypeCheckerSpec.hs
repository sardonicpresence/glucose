module Glucose.TypeCheckerSpec (spec) where

import Test.Prelude

import Control.Monad
import Data.Text (Text)
import Glucose.Identifier
import Glucose.IR
import Glucose.Error
import Glucose.Lexer (tokenise)
import Glucose.Parser (parse)
import Glucose.Parser.Source
import Glucose.Desugar (desugar)
import Glucose.Test.Error
import Glucose.Test.IR.Checked
import qualified Glucose.TypeChecker as TC

spec :: Spec
spec = describe "typeCheck" $ do
  it "passes an empty module" $
    typeCheck "" `shouldBe` Right (fromDefinitions [])
  it "type-checks a module with distinct definitions, aliases and enums" $
    let input = "a=c\nb=3.21\nc=123\ntype it=This|that\nd=a"
        expected = fromDefinitions
          [ alias ("a" `at` "1:1@0-1:1@0") ("c" `at` "1:3@2-1:3@2") Integer
          , constant ("b" `at` "2:1@4-2:1@4") (FloatLiteral 3.21 `at` "2:3@6-2:6@9")
          , constant ("c" `at` "3:1@11-3:1@11") (IntegerLiteral 123 `at` "3:3@13-3:5@15")
          , constructor ("it" `at` "4:6@22-4:7@23") ("This" `at` "4:9@25-4:12@28") 0
          , constructor ("it" `at` "4:6@22-4:7@23") ("that" `at` "4:14@30-4:17@33") 1
          , alias ("d" `at` "5:1@35-5:1@35") ("a" `at` "5:3@37-5:3@37") Integer ]
    in typeCheck input `shouldBe` Right expected
  it "fails a module with duplicate type names" $
    let input = "type A=a\ntype A=b"
    in typeCheck input `shouldErrorWith` compileError "2:6@14" (DuplicateDefinition (Identifier "A") (read "1:6@5"))
  it "fails a module with recursive definitions" $
    let input = "a=c\nb=66\nc=d\nd=a"
    in typeCheck input `shouldErrorWith` compileError "1:1@0" (RecursiveDefinition (Identifier "a"))

typeCheck :: Text -> Either CompileError (Module Checked)
typeCheck = TC.typeCheck <=< desugar <=< uncurry parse <=< tokenise
