module Glucose.DesugarSpec (spec) where

import Test.Prelude

import Control.Monad
import Data.Text (Text)
import Glucose.IR
import Glucose.Compiler (tokenise, parse)
import Glucose.Error
import Glucose.Source
import Glucose.Test.IR.Unchecked hiding (function)
import qualified Glucose.Test.IR.Unchecked as IR (function)
import qualified Glucose.Desugar as Desugar

spec :: Spec
spec = describe "desugar" $ do
  it "transforms an empty module" $
    desugar "" `shouldBe` Right (Module [])
  it "transforms a module with constants, aliases, enums & simple functions" $
    let input = "a=c\nb=3.21\nc=123\ntype it=This|that\nd=a\nid=\\a->a\ntest=\\a->id c"
        expected = Module
          [ alias ("a" `at` "1:1@0") ("c" `at` "1:3@2")
          , constant ("b" `at` "2:1@4") (FloatLiteral 3.21 `at` "2:3@6-2:6@9")
          , constant ("c" `at` "3:1@11") (IntegerLiteral 123 `at` "3:3@13-3:5@15")
          , constructor ("it" `at` "4:6@22-4:7@23") ("This" `at` "4:9@25-4:12@28") 0
          , constructor ("it" `at` "4:6@22-4:7@23") ("that" `at` "4:14@30-4:17@33") 1
          , alias ("d" `at` "5:1@35") ("a" `at` "5:3@37")
          , function ("id" `at` "6:1@39-6:2@40") "6:4@42" ("a" `at` "6:5@43") (reference ("a" `at` "6:8@46"))
          , function ("test" `at` "7:1@48-7:4@51") "7:6@53" ("a" `at` "7:7@54") (apply (reference $ "id" `at` "7:10@57-7:11@58") (reference $ "c" `at` "7:13@60")) ]
    in desugar input `shouldBe` Right expected

desugar :: Text -> Either CompileError (Module Unchecked FromSource)
desugar = Desugar.desugar <=< uncurry parse <=< tokenise

function :: FromSource Text -> String -> FromSource Text -> FromSource (Expression Unchecked FromSource) -> FromSource (Definition Unchecked FromSource)
function name loc = IR.function name (() `at` loc)
