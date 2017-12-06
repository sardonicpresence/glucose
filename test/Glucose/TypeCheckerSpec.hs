module Glucose.TypeCheckerSpec (spec) where

import Test.Prelude

import Control.Comonad.Identity
import Control.Comonad.Utils
import Control.Generalised
import Control.Monad
import Data.Monoid ((<>))
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
  let a = "a"; b = "b"
  it "passes an empty module" $
    typeCheck "" `shouldBe` Right (Module [])
  it "type-checks a module with distinct definitions, aliases and enums" $
    let input = "a=c\nb=3.21\nc=123\ntype it=This|that\nd=a"
        expected = Module
          [ alias ("a" `at` "1:1@0") ("c" `at` "1:3@2") Integer
          , constant ("b" `at` "2:1@4") (FloatLiteral 3.21 `at` "2:3@6-2:6@9")
          , constant ("c" `at` "3:1@11") (IntegerLiteral 123 `at` "3:3@13-3:5@15")
          , constructor ("it" `at` "4:6@22-4:7@23") ("This" `at` "4:9@25-4:12@28") 0
          , constructor ("it" `at` "4:6@22-4:7@23") ("that" `at` "4:14@30-4:17@33") 1
          , alias ("d" `at` "5:1@35") ("a" `at` "5:3@37") Integer ]
     in typeCheck input `shouldBe` Right expected
  it "fails a module with duplicate variable definitions" $
    let input = "a=1\nb=2\na=3"
     in typeCheck input `shouldErrorWith` duplicateDefinition "3:1@8" "a" "1:1@0"
  it "fails a module with duplicate constructor definitions" $
    let input = "type A=a\ntype B=a|b"
     in typeCheck input `shouldErrorWith` duplicateDefinition "2:8@16" "a" "1:8@7"
  it "fails a module with conflicting variable and constructor definitions" $
    let input = "type A=a|b\nb=1"
     in typeCheck input `shouldErrorWith` duplicateDefinition "2:1@11" "b" "1:10@9"
  it "fails a module with duplicate type names" $
    let input = "type A=a\ntype A=b"
     in typeCheck input `shouldErrorWith` duplicateDefinition "2:6@14" "A" "1:6@5"
  it "fails a module with recursive definitions" $
    let input = "a=c\nb=66\nc=d\nd=a"
     in typeCheck input `shouldErrorWith` recursiveDefinition "1:1@0" "a"
  it "errors on constant applicative forms" $
    let input = "id=\\a->a\ncaf=id 0"
     in typeCheck input `shouldErrorWith` TypeCheckError (CAF $ () `at` "2:1@9-2:8@16")
  it "errors on lambdas at local scope" $
    let input = "const=\\a->\\b->a"
     in typeCheck input `shouldErrorWith` TypeCheckError (LocalLambda $ () `at` "1:11@10-1:15@14")
  describe "infers the most general type for simple functions (with normalised polymorphic type names)" $ do
    itDoesSo "for the identity function" $
      typeCheck' "f=\\a->a" `shouldBe` Right (Module
        [ function' "f" "a" a $ reference' (Local "a") a
        ])
    itDoesSo "for a constant function" $
      typeCheck' "f=\\a->1.2" `shouldBe` Right (Module
        [ function' "f" "a" a . pure $ Literal (FloatLiteral 1.2)
        ])
    itDoesSo "for a constant function returning a global" $
      typeCheck' "type T=T\nf=\\a->T" `shouldBe` Right (Module
        [ constructor' "T" "T" 0
        , function' "f" "a" a $ reference' (Global "T") (ADT "T")
        ])
    itDoesSo "for a function applying a constant to a function argument" $ do
      let fn = Integer --> a
      typeCheck' "f=\\g->g 3" `shouldBe` Right (Module
        [ function' "f" "g" fn $
            apply (reference' (Local "g") fn) (pure . Literal $ IntegerLiteral 3) a
        ])
    itDoesSo "for a function applying a global to a function argument" $ do
      let fn = Float --> a
      typeCheck' "f=\\g->g a\na=0.9" `shouldBe` Right (Module
        [ function' "f" "g" fn $
            apply (reference' (Local "g") fn) (reference' (Global "a") Float) a
        , constant' "a" (FloatLiteral 0.9)
        ])
    itDoesSo "for functions returning functions (and calling them)" $ do
      let fn = a --> a
      typeCheck' "f=\\a->a\ng=\\a->f\nh=\\a->g a a" `shouldBe` Right (Module
        [ function' "f" "a" a $ reference' (Local "a") a
        , function' "g" "a" a $ reference' (Global "f") (b --> b)
        , function' "h" "a" a $
            let g_a = apply (reference' (Global "g") (a --> fn)) (reference' (Local "a") a) fn
             in apply g_a (reference' (Local "a") a) a
        ])
    itDoesSo "when passing functions as arguments" $
      typeCheck' "f=\\a->a\ng=\\h->h 3" `shouldBe` Right (Module
        [ function' "f" "a" a $ reference' (Local "a") a
        , function' "g" "h" (Integer --> a) $
            apply (reference' (Local "h") (Integer --> a)) (pure . Literal $ IntegerLiteral 3) a
        ])
  it "produces the most general type for chained application" $
    typeCheck' "id=\\a->a\ntest=\\f->f id 3" `shouldBe` Right (Module
      [ function' "id" "a" a $ local' "a" a
      , function' "test" "f" ((a --> a) --> Integer --> b) $
          apply' (apply' (local' "f") (global' "id") (a --> a)) (const $ integer' 3) Integer b
      ])
  describe "maintains both unified representations for codegen" $ do
    let forAllTestCases :: Applicative f => (Text -> Text -> (DataType (Type Checked) -> f (Expression Checked f)) -> DataType (Type Checked) -> Expectation) -> Expectation
        forAllTestCases testCase = do
          testCase "" "2" (const $ integer' 2) Integer
          testCase "a=4" "a" (global' "a") Integer
          testCase "" "1.2" (const $ float' 1.2) Float
          testCase "a=2.3" "a" (global' "a") Float
          testCase "type T=T" "T" (global' "T") (ADT "T")
          testCase "a=\\a->a" "a" (global' "a") (b --> b)
    itDoesSo "when supplying concrete values to a polymorphic function" $
      forAllTestCases $ \decl val expr ty ->
        let Right (Module decls) = typeCheck' decl in
        typeCheck' (decl <> "\nf=\\_->3\ng=\\_->f " <> val) `shouldBe` Right (Module $ decls ++
          [ function' "f" "_" a $ integer' 3
          , function' "g" "_" a $ apply (global' "f" $ Constrained ty --> Integer) (expr ty) Integer
          ])
    itDoesSo "when expecting concrete values from a polymorphic function" $
      forAllTestCases $ \decl val expr ty ->
        let Right (Module decls) = typeCheck' decl in
        typeCheck' (decl <> "\nf=\\_->_\ng=\\_->f " <> val) `shouldBe` Right (Module $ decls ++
          [ function' "f" "_" a $ local' "_" a
          , function' "g" "_" a $ apply (global' "f" $ Constrained ty --> Constrained ty) (expr ty) ty
          ])
  describe "errors on function application on non-functions" $ do
    itDoesSo "for literals" $
      typeCheck' "f=\\b->3 b" `shouldErrorWith` expectedFunction "a" "b" (Integer `at` "1:7@6")
    itDoesSo "for global constants" $
      typeCheck' "a=2\nf=\\b->a b" `shouldErrorWith` expectedFunction "a" "b" (Integer `at` "2:7@10")
    itDoesSo "for return values" $
      typeCheck' "f=\\a->7\ng=\\b->f b b" `shouldErrorWith` expectedFunction "a" "b"
        (Integer `at` "2:7@14-2:9@16")
  describe "errors on argument type mismatch" $ do
    itDoesSo "passing a non-function instead of a function" $
      typeCheck' "f=\\g->g 1\nh=\\a->f 2" `shouldErrorWith` expectedFunction Integer "a"
        (Integer `at` "2:9@18")
    itDoesSo "passing a function of the wrong type" $
      typeCheck' "f=\\g->g 1\ng=\\f->f 2\nh=\\a->f g" `shouldErrorWith` typeMismatch
        (Integer --> "a")
        ((Integer --> "a") --> "a" `at` "3:9@28")

typeCheck :: Text -> Either CompileError (Module Checked FromSource)
typeCheck = TC.typeCheck <=< desugar <=< uncurry parse <=< tokenise

typeCheck' :: Text -> Either CompileError (Module Checked Identity)
typeCheck' = (rewrap forget <$>) . typeCheck

duplicateDefinition :: String -> Text -> String -> CompileError
duplicateDefinition loc name prev = TypeCheckError $ DuplicateDefinition (Identifier name `at` loc) (() `at` prev)

recursiveDefinition :: String -> Text -> CompileError
recursiveDefinition loc name = TypeCheckError $ RecursiveDefinition $ Identifier name `at` loc

typeMismatch :: DataType (Type Checked) -> FromSource (DataType (Type Checked)) -> CompileError
typeMismatch expected actual = TypeCheckError $ TypeMismatch (CheckedType expected) (CheckedType <$> actual)

expectedFunction :: DataType (Type Checked) -> DataType (Type Checked) -> FromSource (DataType (Type Checked)) -> CompileError
expectedFunction from to = typeMismatch $ from --> to
