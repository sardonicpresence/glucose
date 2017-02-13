module Glucose.Codegen.LLVM.RT
(
  functionDeclarations, heapAlloc, heapAllocN,
  Representation(..), GeneratedFn(..), ApplyFn(..), ApplyType(..), generateFunction, generatedName, typeRep
) where

import Control.Monad.Identity
import Data.Text (pack)
import Glucose.Codegen.LLVM.Types
import Glucose.VarGen
import LLVM.AST
import LLVM.DSL
import LLVM.Name

-- * Built-in runtime functions

_heapAlloc :: (Name, Type)
_heapAlloc = (rtName "heapAlloc", Function box [I 64])

functionDeclarations :: [Global]
functionDeclarations = map (uncurry FunctionDeclaration) [ _heapAlloc ]

heapAlloc :: Monad m => Expression -> LLVMT m Expression
heapAlloc bytes = callFn _heapAlloc [bytes]

heapAllocN :: Monad m => Int -> LLVMT m Expression
heapAllocN = heapAlloc . i64

callFn :: Monad m => (Name, Type) -> [Expression] -> LLVMT m Expression
callFn (name, ty) = call $ GlobalReference name ty

-- * Generated Functions

data GeneratedFn = GeneratedApply ApplyFn
  deriving (Eq, Ord)

data ApplyFn = ApplyFn ApplyType Representation [Representation]
  deriving (Eq, Ord)

data ApplyType = ApplyUnknown | ApplyPartial -- TODO
  deriving (Eq, Ord)

data Representation = I32Rep | F64Rep | BoxRep
  deriving (Eq, Ord)

typeRep :: Type -> Representation
typeRep (I _) = I32Rep
typeRep F64 = F64Rep
typeRep _ = BoxRep

repType :: Representation -> Type
repType I32Rep = I 32
repType F64Rep = F64
repType BoxRep = box

functionType :: Representation -> [Representation] -> Type
functionType result args = Ptr $ Function (repType result) (map repType args)

argNames :: [Name]
argNames = map Name variables

repCode :: Representation -> Char
repCode I32Rep = 'I'
repCode F64Rep = 'F'
repCode BoxRep = 'P'

generatedArgs :: [Representation] -> [Arg]
generatedArgs = zipWith Arg argNames . map repType

generatedName :: GeneratedFn -> Name
generatedName (GeneratedApply (ApplyFn ApplyUnknown resultType argTypes)) =
  rtName . pack $ "uapply" ++ map repCode (resultType : argTypes)

generateFunction :: GeneratedFn -> Global
generateFunction = evalLLVM . \case
  toGen@(GeneratedApply (ApplyFn ApplyUnknown resultType argTypes)) ->
    let fn = Arg "fn" box
        args = generatedArgs argTypes
     in singleFunctionDefinition (generatedName toGen) LinkOnceODR (args ++ [fn]) $ do
          toMask <- ptrtoint (argReference fn) size
          tag <- andOp toMask $ i32 tagMask
          isTrivial <- icmp Eq tag (i32 $ length args)
          br isTrivial "Trivial" "Nontrivial"
          label "Trivial"
          fp <- bitcast (argReference fn) $ functionType resultType argTypes
          ret =<< call fp (map argReference args)
          label "Nontrivial"
          isClosure <- icmp Eq tag $ i32 0
          br isClosure "Closure" "Function"
          label "Function"
          unreachable -- TODO
          label "Closure"
          unreachable -- TODO
