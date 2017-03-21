module Glucose.Codegen.LLVM.RT
(
  functionDeclarations, heapAlloc, heapAllocN,
  Representation(..), GeneratedFn(..), ApplyFn(..), ApplyType(..), generateFunction, generatedName, typeRep
) where

import Data.Foldable
import Data.Text (pack)
import Glucose.Codegen.LLVM.Types
import Glucose.VarGen
import LLVM.AST
import LLVM.DSL
import LLVM.Name

-- * Built-in runtime functions

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

_heapAlloc :: (Name, Parameter Type, [Type], FunctionAttributes)
_heapAlloc = (rtName "heapAlloc", Parameter box True True, [I 64], FunctionAttributes (Just 0))

_memcpy :: (Name, Parameter Type, [Type], FunctionAttributes)
_memcpy = ("llvm.memcpy.p0i8.p0i8.i64", Parameter Void False False, [Ptr (I 8), Ptr (I 8), I 64, I 32, I 1], FunctionAttributes Nothing)

functionDeclarations :: [Global]
functionDeclarations = map (uncurry4 FunctionDeclaration) [ _memcpy, _heapAlloc ]

heapAlloc :: Monad m => Expression -> LLVMT m Expression
heapAlloc bytes = callFn _heapAlloc [bytes]

heapAllocN :: Monad m => Int -> LLVMT m Expression
heapAllocN = heapAlloc . i64

memcpy :: Monad m => Expression -> Expression -> Expression -> Int -> LLVMT m ()
memcpy to from bytes align = do
  to' <- bitcast to (Ptr $ I 8)
  from' <- bitcast from (Ptr $ I 8)
  callFn_ _memcpy [to', from', bytes, i32 align, integer (I 1) 0]

callFn :: Monad m => (Name, Parameter Type, [Type], FunctionAttributes) -> [Expression] -> LLVMT m Expression
callFn (name, result, args, _) = call $ GlobalReference name $ Function (parameter result) args

callFn_ :: Monad m => (Name, Parameter Type, [Type], FunctionAttributes) -> [Expression] -> LLVMT m ()
callFn_ (name, result, args, _) = call_ $ GlobalReference name $ Function (parameter result) args

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
          fp <- flip inttoptr (functionType resultType argTypes) =<< subOp toMask tag
          ret =<< call fp (map argReference args)

          label "Nontrivial"
          isClosure <- icmp Eq tag $ i32 0
          br isClosure "Closure" "Function"

          label "Function"
          unreachable -- TODO

          label "Closure"
          -- Tag bits were 0 so no need to mask
          pclosure <- bitcast (argReference fn) (Ptr closure)
          napplied <- flip zext (I 64) =<< load =<< getElementPtr pclosure [i64 0, i32 2]
          papplied <- getElementPtr pclosure [i64 0, i32 3, i32 0]
          ntotal <- addOp napplied $ i64 (length args)

          -- Determine number of bytes per argument
          papplied2 <- getElementPtr pclosure [i64 0, i32 3, i32 1]
          p1 <- ptrtoint papplied size
          p2 <- ptrtoint papplied2 size
          bytesPer <- subOp p2 p1

          -- Allocate space for argument stack
          bytes <- mulOp ntotal bytesPer
          ptotal <- flip bitcast (Ptr box) =<< heapAlloc bytes

          -- Push previously applied arguments
          bytesApplied <- mulOp napplied bytesPer
          memcpy ptotal papplied bytesApplied 16

          -- Push new arguments
          for_ [0..length args - 1] $ \i -> do
            let arg = args !! i
            iarg <- addOp napplied (i64 $ i+1)
            parg <- flip bitcast (Ptr $ typeOf arg)  =<< getElementPtr ptotal [iarg]
            store (argReference arg) parg

          -- Make the call
          let tyTarget = Function (repType resultType) [Ptr box]
          target <- flip bitcast (Ptr tyTarget) =<< load =<< getElementPtr pclosure [i64 0, i32 0]
          ret =<< call target [ptotal]
