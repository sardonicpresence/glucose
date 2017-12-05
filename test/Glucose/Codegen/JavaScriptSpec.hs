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
  let a = "a"; b = "b"; c = "c"; d = "d"
  it "compiles an empty module" $
    codegenModule (Module [] :: Module Identity) `shouldShow` ""
  it "compiles global numeric constant definitions correctly" $
    codegenDefinitions [constant' "a" $ IR.IntegerLiteral 123,
                        constant' "b" $ IR.FloatLiteral 3.21] `shouldShow`
      "a = 123\nb = 3.21\n"
  it "compiles global aliases correctly" $
    codegenDefinitions
      [ alias' "a" "b" Integer -- Alias to as-yet-undefined alias
      , alias' "b" "c" Integer -- Alias to external definition
      , alias' "d" "e" Integer -- Alias to as-yet-undefined constructor
      , constructor' "test" "e" 0 ]
        `shouldShow` unlines
      [ "b = c"
      , "a = b"
      , "test = function() {}", "e = new test()"
      , "d = e" ]
  it "compiles enum constructors correctly" $
    codegenDefinitions
      [ constructor' "test" "a" 0
      , constructor' "test" "B" 1]
        `shouldShow` unlines
      [ "test = function() {}"
      , "a = new test()"
      , "B = new test()" ]
  it "compiles simple functions correctly" $
    codegenDefinitions
      [ function' "f" "a" a $ local' "a" a
      , function' "g" "a" a $ global' "b" b
      , function' "h" "a" a $ global' "c" Integer
      , function' "i" "a" a $ integer' 42
      , function' "j" "a" a $ global' "f" (a --> b)
      , function' "k" "f" (a --> b) $ local' "f" (a --> b) ]
        `shouldShow` unlines
      [ "function f(a) { return a }"
      , "function g(a) { return b }"
      , "function h(a) { return c }"
      , "function i(a) { return 42 }"
      , "function j(a) { return f }"
      , "function k(f) { return f }" ]
  it "compiles function calls correctly" $
    codegenDefinitions
      [ function' "f1" "a" a $ apply' (global' "g") (local' "a") a b
      , function' "f2" "a" a $ apply' (apply' (global' "h") (local' "a") a) (global' "g") (b --> c) d
      , function' "f3" "f" (Integer --> a) $ apply' (local' "f") (const $ integer' 3) Integer a
      , function' "f4" "f" (a --> b) $ apply' (apply' (global' "g") (local' "f") (a --> b)) (const $ integer' 3) Integer c ]
        `shouldShow` unlines
      [ "function f1(a) { return g(a) }"
      , "function f2(a) { return h(a)(g) }"
      , "function f3(f) { return f(3) }"
      , "function f4(f) { return g(f)(3) }" ]
  it "mangles reserved words" $
    codegenDefinitions
      [ alias' "const" "null" $ a --> b
      , alias' "this" "with" Integer
      , function' "true" "class" a $ apply' (global' "catch") (local' "class") a b ]
        `shouldShow` unlines
      [ "$const = $null"
      , "$this = $with"
      , "function $true($class) { return $catch($class) }" ]

codegenDefinitions :: [Identity (Definition Identity)] -> JavaScript
codegenDefinitions = JS.codegenDefinitions . map extract
