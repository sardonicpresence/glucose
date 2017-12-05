module Glucose.Codegen.LLVMSpec (spec) where

import Test.Prelude

import Control.Comonad
import Control.Comonad.Identity
import Data.Maybe (fromJust)
import Glucose.IR.Checked as IR
import Glucose.Codegen.LLVM as LLVM hiding (codegenDefinitions)
import qualified Glucose.Codegen.LLVM as LLVM (codegenDefinitions)
import Glucose.Codegen.LLVM.Types (box)
import Glucose.Codegen.Target
import Glucose.Test.IR.Checked
import LLVM.AST as LLVM
import LLVM.DSL hiding (functionDefinition)
import LLVM.Name

spec :: Spec
spec = describe "LLVM codegen" $ do
  let a = "a"; b = "b"; c = "c"
  it "compiles an empty module" $
    codegenModule target (IR.Module [] :: IR.Module Identity) `shouldBe` LLVM.Module (llvmTarget target) []
  it "compiles global numeric constant definitions correctly" $
    codegenDefinitions [constant' "a" $ IR.IntegerLiteral 123,
                        constant' "b" $ IR.FloatLiteral 3.21] `shouldBe`
      [LLVM.VariableDefinition (Name "a") LLVM.External Unnamed (i32 123) alignment,
       LLVM.VariableDefinition (Name "b") LLVM.External Unnamed (f64 3.21) alignment]
  it "compiles global aliases correctly" $
    codegenDefinitions [alias' "a" "b" Integer] `shouldBe`
      [LLVM.Alias (Name "a") LLVM.External Unnamed (GlobalReference (Name "b") (LLVM.I 32)) (LLVM.I 32)]
  it "compiles enum constructors correctly" $
    codegenDefinitions [constructor' "test" "a" 0, constructor' "test" "B" 1] `shouldBe`
      [LLVM.VariableDefinition (Name "a") LLVM.External Unnamed (i32 0) alignment,
       LLVM.VariableDefinition (Name "B") LLVM.External Unnamed (i32 1) alignment]
  it "mangles global identifiers" $
    codegenDefinitions [constant' "-._$\x05d0\&azAZ09\x5d5_" $ IR.IntegerLiteral 0] `shouldBe`
      [LLVM.VariableDefinition (Name "-._$24$$5d0$azAZ09$5d5$_") LLVM.External Unnamed (i32 0) alignment]
  it "compiles simple functions correctly" $
    codegenDefinitions
      [ function' "id" "a" a $ local' "a" a
      , function' "const" "a" a $ global' "id" (b --> b)
      , function' "three" "a" a $ apply' (global' "id") (const $ integer' 3) (Constrained Integer) (Constrained Integer)
      , function' "call1" "f" (Integer --> a) $ apply' (local' "f") (const $ integer' 1) Integer a ]
        `shouldBe`
      [ functionDefinition "id" [LLVM.Arg "a" box] $ ret (LocalReference "a" box)
      , functionDefinition "const" [LLVM.Arg "a" box] $ ret (GlobalReference "id" $ box ~~> box)
      , functionDefinition "three" [LLVM.Arg "a" box] $ do
          arg <- inttoptr (i32 3) box -- Primitive arguments passed as boxes via bitcast
          result <- call FastCC (GlobalReference "id" $ box ~~> box) [arg]
          ret =<< ptrtoint result (I 32)
      , functionDefinition "call1" [LLVM.Arg "f" $ I 32 ~~> box] $
          ret =<< call FastCC (LocalReference "f" $ I 32 ~~> box) [i32 1]
      ]
  it "inserts loads for global references (when necessary)" $
    codegenDefinitions
      [ function' "test" "f" (a --> a) $ global' "a" Integer
      , function' "test2" "f" (a --> a) $ global' "a" Float
      , function' "test3" "f" (a --> a) $ global' "a" (ADT "A")
      , function' "test4" "f" (a --> a) $ global' "a" (b --> c) ]
        `shouldBe`
      [ functionDefinition "test" [LLVM.Arg "f" $ box ~~> box] $
          ret =<< load (GlobalReference "a" . Ptr $ I 32)
      , functionDefinition "test2" [LLVM.Arg "f" $ box ~~> box] $
          ret =<< load (GlobalReference "a" . Ptr $ F64)
      , functionDefinition "test3" [LLVM.Arg "f" $ box ~~> box] $
          ret $ GlobalReference "a" (Ptr $ I 32)
      , functionDefinition "test4" [LLVM.Arg "f" $ box ~~> box] $
          ret $ GlobalReference "a" (box ~~> box)
      ]
  it "bitcasts primitives to/from boxes when passed/received as polymorphic arguments/return-values" $
    codegenDefinitions
      [ function' "test" "f" (a --> a) $ apply' (local' "f") (const $ integer' 13) (Constrained Integer) (Constrained Float)
      , function' "test2" "f" (a --> a) $ apply' (local' "f") (const $ float' 9.8) (Constrained Float) (Constrained Integer)
      ]
        `shouldBe`
      [ functionDefinition "test" [LLVM.Arg "f" $ box ~~> box] $ do
          arg <- inttoptr (i32 13) box
          result <- call FastCC (LocalReference "f" $ box ~~> box) [arg]
          ret =<< ptrtofp result F64
      , functionDefinition "test2" [LLVM.Arg "f" $ box ~~> box] $ do
          arg <- fptoptr (f64 9.8) box
          result <- call FastCC (LocalReference "f" $ box ~~> box) [arg]
          ret =<< ptrtoint result (I 32)
      ]
  it "compiles chained application correctly" $
    codegenDefinitions
      [ function' "test" "f" ((a --> a) --> Integer --> b) $ apply' (apply' (local' "f") (global' "id") (a --> a)) (global' "b") Integer b ]
        `shouldBe`
      [ functionDefinition "test" [LLVM.Arg "f" $ (box ~~> box) ~~> I 32 ~~> box] $ do
          g <- call FastCC (LocalReference "f" $ (box ~~> box) ~~> I 32 ~~> box) [GlobalReference "id" $ box ~~> box]
          h <- load $ GlobalReference "b" . Ptr $ I 32
          ret =<< call FastCC g [h]
      ]

codegenDefinitions :: [Identity (Definition Identity)] -> [Global]
codegenDefinitions = LLVM.codegenDefinitions . map extract

alignment = Alignment 0
target = fromJust $ parseTriple "x86_64-pc-mingw32"

functionDefinition f as = runIdentity . singleFunctionDefinition (Name f) External FastCC as (FunctionAttributes Unnamed [] [0] alignment)

infixr 8 ~~>
from ~~> to = Ptr $ LLVM.Function to [from]
