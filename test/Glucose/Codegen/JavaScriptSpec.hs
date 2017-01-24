module Glucose.Codegen.JavaScriptSpec (spec) where

import Test.Prelude

import Glucose.IR as IR
import Glucose.Codegen.JavaScript
import Glucose.Test.IR

spec :: Spec
spec = describe "JavaScript codegen" $ do
  it "compiles an empty module" $
    codegen (IR.Module []) `shouldShow` ""
  it "compiles global numeric constant definitions correctly" $
    codegenDefinitions [constant "a" $ IR.IntegerLiteral 123,
                        constant "b" $ IR.FloatLiteral 3.21] `shouldShow`
      "a = 123\nb = 3.21\n"
  it "compiles global aliases correctly" $
    codegenDefinitions [alias "a" "b" Integer] `shouldShow` "a = b\n"
  -- TODO: JavaScript name mangling e.g. keywords
