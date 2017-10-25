module Glucose.TypeCheckerSpec (spec) where

import Test.Prelude

import Control.Comonad.Identity
import Control.Comonad.Utils
import Control.Generalised
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
          [ alias ("a" `at` "1:1@0") ("c" `at` "1:3@2") (Unboxed Integer)
          , constant ("b" `at` "2:1@4") (FloatLiteral 3.21 `at` "2:3@6-2:6@9")
          , constant ("c" `at` "3:1@11") (IntegerLiteral 123 `at` "3:3@13-3:5@15")
          , constructor ("it" `at` "4:6@22-4:7@23") ("This" `at` "4:9@25-4:12@28") 0
          , constructor ("it" `at` "4:6@22-4:7@23") ("that" `at` "4:14@30-4:17@33") 1
          , alias ("d" `at` "5:1@35") ("a" `at` "5:3@37") (Unboxed Integer) ]
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
    let a = Polymorphic "a"
    let b = Polymorphic "b"
    it "does so for the identity function" $
      typeCheck' "f=\\a->a" `shouldBe` Right (Module
        [ functionAnywhere "f" "a" a $ referenceAnywhere Local "a" a
        ])
    it "does so for a constant function" $
      typeCheck' "f=\\a->1.2" `shouldBe` Right (Module
        [ functionAnywhere "f" "a" a . pure $ Literal (FloatLiteral 1.2)
        ])
    it "does so for a constant function returning a global" $
      typeCheck' "type T=T\nf=\\a->T" `shouldBe` Right (Module
        [ constructorAnywhere "T" "T" 0
        , functionAnywhere "f" "a" a $ referenceAnywhere Global "T" (ADT "T")
        ])
    it "does so for a function applying a constant to a function argument" $ do
      let fn = functionType (Unboxed Integer) a
      typeCheck' "f=\\g->g 3" `shouldBe` Right (Module
        [ functionAnywhere "f" "g" fn $
            apply (referenceAnywhere Local "g" fn) (pure . Literal $ IntegerLiteral 3) a
        ])
    it "does so for a function applying a global to a function argument" $ do
      let fn = functionType (Unboxed Float) a
      typeCheck' "f=\\g->g a\na=0.9" `shouldBe` Right (Module
        [ functionAnywhere "f" "g" fn $
            apply (referenceAnywhere Local "g" fn) (referenceAnywhere Global "a" (Unboxed Float)) a
        , constantAnywhere "a" (FloatLiteral 0.9)
        ])
    it "does so for functions returning functions (and calling them)" $ do
      let fn = functionType a a
      typeCheck' "f=\\a->a\ng=\\a->f\nh=\\a->g a a" `shouldBe` Right (Module
        [ functionAnywhere "f" "a" a $ referenceAnywhere Local "a" a
        , functionAnywhere "g" "a" a $ referenceAnywhere Global "f" (functionType b b)
        , functionAnywhere "h" "a" a $
            let g_a = apply (referenceAnywhere Global "g" $ functionType a fn) (referenceAnywhere Local "a" a) fn
             in apply g_a (referenceAnywhere Local "a" a) a
        ])
    it "does so when passing functions as arguments" $
      typeCheck' "f=\\a->a\ng=\\h->h 3" `shouldBe` Right (Module
        [ functionAnywhere "f" "a" a $ referenceAnywhere Local "a" a
        , functionAnywhere "g" "h" (functionType (Unboxed Integer) a) $
            apply (referenceAnywhere Local "h" $ functionType (Unboxed Integer) a) (pure . Literal $ IntegerLiteral 3) a
        ])
  describe "errors on function application on non-functions" $ do
    let nonFunctionError from to = typeMismatch $ Function (Arity 1) (CheckedType from) (CheckedType to)
    it "does so for literals" $
      typeCheck' "f=\\b->3 b" `shouldErrorWith` nonFunctionError "a" "b" (Unboxed Integer `at` "1:7@6")
    it "does so for global constants" $
      typeCheck' "a=2\nf=\\b->a b" `shouldErrorWith` nonFunctionError "a" "b" (Unboxed Integer `at` "2:7@10")
    it "does so for return values" $
      typeCheck' "f=\\a->7\ng=\\b->f b b" `shouldErrorWith` nonFunctionError "a" "b" (Unboxed Integer `at` "2:7@14-2:9@16")

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
