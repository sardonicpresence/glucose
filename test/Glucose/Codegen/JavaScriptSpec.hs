module Glucose.Codegen.JavaScriptSpec (spec) where

import Test.Prelude

import Glucose.IR as IR
import Glucose.Codegen.JavaScript
import Glucose.Test.IR.Checked

spec :: Spec
spec = describe "JavaScript codegen" $ do
  it "compiles an empty module" $
    codegen (IR.Module []) `shouldShow` ""
  it "compiles global numeric constant definitions correctly" $
    codegen (IR.Module [constantAnywhere "a" $ IR.IntegerLiteral 123,
                        constantAnywhere "b" $ IR.FloatLiteral 3.21]) `shouldShow`
      "a = 123\nb = 3.21\n"
  it "compiles global aliases correctly" $
    codegen (IR.Module [aliasAnywhere "a" "b" Integer]) `shouldShow` "a = b\n"
  it "compiles enum constructors correctly" $
    codegen (IR.Module [constructorAnywhere "test" "a" 0, constructorAnywhere "test" "B" 1]) `shouldShow` unlines
    [ "test = function() {}"
    , "a = new test()"
    , "B = new test()" ]
  -- TODO: JavaScript name mangling e.g. keywords
