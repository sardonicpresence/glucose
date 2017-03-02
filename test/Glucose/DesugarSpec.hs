module Glucose.DesugarSpec (spec) where

import Test.Prelude

import Control.Monad
import Data.Text (Text)
import Glucose.Identifier
import Glucose.IR
import Glucose.Error
import Glucose.Lexer (tokenise)
import Glucose.Parser (parse)
import Glucose.Parser.Source
import Glucose.Test.Error
import Glucose.Test.IR.Unchecked
import qualified Glucose.Desugar as Desugar

spec :: Spec
spec = describe "desugar" $ do
  it "transforms an empty module" $
    desugar "" `shouldBe` Right (fromDefinitions [])
  it "transforms a module with constants, aliases & enums" $
    let input = "a=c\nb=3.21\nc=123\ntype it=This|that\nd=a"
        expected = fromDefinitions
          [ alias ("a" `at` "1:1@0-1:1@0") ("c" `at` "1:3@2-1:3@2")
          , constant ("b" `at` "2:1@4-2:1@4") (FloatLiteral 3.21 `at` "2:3@6-2:6@9")
          , constant ("c" `at` "3:1@11-3:1@11") (IntegerLiteral 123 `at` "3:3@13-3:5@15")
          , constructor ("it" `at` "4:6@22-4:7@23") ("This" `at` "4:9@25-4:12@28") 0
          , constructor ("it" `at` "4:6@22-4:7@23") ("that" `at` "4:14@30-4:17@33") 1
          , alias ("d" `at` "5:1@35-5:1@35") ("a" `at` "5:3@37-5:3@37") ]
    in desugar input `shouldBe` Right expected
  it "fails a module with duplicate variable definitions" $
    let input = "a=1\nb=2\na=3"
    in desugar input `shouldErrorWith` compileError "3:1@8" (DuplicateDefinition (Identifier "a") (read "1:1@0"))
  it "fails a module with duplicate constructor definitions" $
    let input = "type A=a\ntype B=a|b"
    in desugar input `shouldErrorWith` compileError "2:8@16" (DuplicateDefinition (Identifier "a") (read "1:8@7"))
  it "fails a module with conflicting variable and constructor definitions" $
    let input = "type A=a|b\nb=1"
    in desugar input `shouldErrorWith` compileError "2:1@11" (DuplicateDefinition (Identifier "b") (read "1:10@9"))

desugar :: Text -> Either CompileError (Module Unchecked)
desugar = Desugar.desugar <=< uncurry parse <=< tokenise
