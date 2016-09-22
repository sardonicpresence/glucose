module Glucose.Codegen.JavaScriptSpec (spec) where

import Test.Prelude

import Control.Comonad
import Glucose.IR.Checked as IR
import Glucose.Codegen.JavaScript hiding (codegenDefinitions)
import qualified  Glucose.Codegen.JavaScript as JS (codegenDefinitions)
import Glucose.Parser.Source
import Glucose.Test.IR.Checked

spec :: Spec
spec = describe "JavaScript codegen" $ do
  it "compiles an empty module" $
    codegen (Module []) `shouldShow` ""
  it "compiles global numeric constant definitions correctly" $
    codegenDefinitions [constantAnywhere "a" $ IR.IntegerLiteral 123,
                        constantAnywhere "b" $ IR.FloatLiteral 3.21] `shouldShow`
      "a = 123\nb = 3.21\n"
  it "compiles global aliases correctly" $
    let input =
          [ aliasAnywhere "a" "b" Integer -- Alias to as-yet-undefined alias
          , aliasAnywhere "b" "c" Integer -- Alias to external definition
          , aliasAnywhere "d" "e" Integer -- Alias to as-yet-undefined constructor
          , constructorAnywhere "test" "e" 0 ]
     in codegenDefinitions input `shouldShow` "b = c\na = b\ntest = function() {}\ne = new test()\nd = e\n"
  it "compiles enum constructors correctly" $
    codegenDefinitions [constructorAnywhere "test" "a" 0, constructorAnywhere "test" "B" 1] `shouldShow` unlines
    [ "test = function() {}"
    , "a = new test()"
    , "B = new test()" ]
  -- TODO: JavaScript name mangling e.g. keywords

codegenDefinitions :: [FromSource Definition] -> JSRaw
codegenDefinitions = JS.codegenDefinitions . map extract
