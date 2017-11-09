module Glucose.Codegen.JavaScriptSpec (spec) where

import Test.Prelude

import Control.Comonad
import Control.Comonad.Identity
import Glucose.IR.Checked as IR
import Glucose.Codegen.JavaScript hiding (codegenDefinitions)
import qualified  Glucose.Codegen.JavaScript as JS (codegenDefinitions)
import Glucose.Test.IR.Checked
import JavaScript.AST (JavaScript)

spec :: Spec
spec = describe "JavaScript codegen" $ do
  it "compiles an empty module" $
    codegenModule (Module [] :: Module Identity) `shouldShow` ""
  it "compiles global numeric constant definitions correctly" $
    codegenDefinitions [constantAnywhere "a" $ IR.IntegerLiteral 123,
                        constantAnywhere "b" $ IR.FloatLiteral 3.21] `shouldShow`
      "a = 123\nb = 3.21\n"
  it "compiles global aliases correctly" $
    codegenDefinitions
      [ aliasAnywhere "a" "b" $ Unboxed Integer -- Alias to as-yet-undefined alias
      , aliasAnywhere "b" "c" $ Unboxed Integer -- Alias to external definition
      , aliasAnywhere "d" "e" $ Unboxed Integer -- Alias to as-yet-undefined constructor
      , constructorAnywhere "test" "e" 0 ]
        `shouldShow` unlines
      [ "b = c"
      , "a = b"
      , "test = function() {}", "e = new test()"
      , "d = e" ]
  it "compiles enum constructors correctly" $
    codegenDefinitions
      [ constructorAnywhere "test" "a" 0
      , constructorAnywhere "test" "B" 1]
        `shouldShow` unlines
      [ "test = function() {}"
      , "a = new test()"
      , "B = new test()" ]
  it "compiles simple functions correctly" $ do
    let a = Polymorphic "a"; b = Polymorphic "b"
    codegenDefinitions
      [ functionAnywhere "f" "a" a $ referenceAnywhere (Local "a") a
      , functionAnywhere "g" "a" a $ referenceAnywhere (Global "b") b
      , functionAnywhere "h" "a" a $ referenceAnywhere (Global "c") (Unboxed Integer)
      , functionAnywhere "i" "a" a $ literalAnywhere (IR.IntegerLiteral 42)
      , functionAnywhere "j" "a" a $ referenceAnywhere (Global "f") (functionType a b)
      , functionAnywhere "k" "f" (functionType a b) $ referenceAnywhere (Local "f") (functionType a b) ]
        `shouldShow` unlines
      [ "function f(a) { return a }"
      , "function g(a) { return b }"
      , "function h(a) { return c }"
      , "function i(a) { return 42 }"
      , "function j(a) { return f }"
      , "function k(f) { return f }" ]
  it "compiles function calls correctly" $ do
    let a = Polymorphic "a"; b = Polymorphic "b"; c = Polymorphic "c"; d = Polymorphic "d"
    let g = referenceAnywhere (Global "g") (functionType a b)
    -- let h = referenceAnywhere (Global "h") (functionType a $ functionType b c)
    codegenDefinitions
      [ functionAnywhere "f1" "a" a $ apply g (referenceAnywhere (Local "a") a) b
      , functionAnywhere "f2" "a" a $ apply
          (apply (referenceAnywhere (Global "h") (functionType a (functionType (functionType b c) d))) (referenceAnywhere (Local "a") a) (functionType (functionType b c) d))
          g d
      ]
        `shouldShow` unlines
      [ "function f1(a) { return g(a) }"
      , "function f2(a) { return h(a)(g) }" ]

  -- TODO: JavaScript name mangling e.g. keywords

codegenDefinitions :: [Identity (Definition Identity)] -> JavaScript
codegenDefinitions = JS.codegenDefinitions . map extract
