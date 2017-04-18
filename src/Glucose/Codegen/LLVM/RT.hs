module Glucose.Codegen.LLVM.RT
(
  functionDeclarations, heapAlloc, heapAllocN,
  Representation(..), GeneratedFn(..), ApplyFn(..), ApplyType(..),
  generateFunction, generatedName, generatedType, typeRep
) where

import Data.Foldable
import Data.Text (pack)
import Data.Traversable
import Glucose.Codegen.LLVM.Types
import Glucose.VarGen
import LLVM.AST
import LLVM.DSL
import LLVM.Name

-- * Built-in runtime functions

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d

_heapAlloc :: (Name, Parameter Type, [Type], FunctionAttributes)
_heapAlloc = (rtName "heapAlloc", Parameter box True True, [size], FunctionAttributes (Just 0))

_memcpy :: (Name, Parameter Type, [Type], FunctionAttributes)
-- _memcpy = ("llvm.memcpy.p0i8.p0i8.i64", Parameter Void False False, [Ptr (I 8), Ptr (I 8), I 64, I 32, I 1], FunctionAttributes Nothing)
_memcpy = ("memcpy", Parameter Void False False, [Ptr (I 8), Ptr (I 8), size], FunctionAttributes Nothing)

_assume :: (Name, Parameter Type, [Type], FunctionAttributes)
_assume = ("llvm.assume", Parameter Void False False, [I 1], FunctionAttributes Nothing)

functionDeclarations :: [Global]
functionDeclarations = map (uncurry4 FunctionDeclaration) [ _assume, _memcpy, _heapAlloc ]

heapAlloc :: Monad m => Expression -> LLVMT m Expression
heapAlloc bytes = do
  ptr <- callFn _heapAlloc [bytes]
  -- assume =<< icmp Eq (integer size 0) =<< andOp (integer size 15) =<< ptrtoint ptr size
  pure ptr

heapAllocN :: Monad m => Int -> LLVMT m Expression
heapAllocN = heapAlloc . integer size

assume :: Monad m => Expression -> LLVMT m ()
assume cond = callFn_ _assume [cond]

memcpy :: Monad m => Expression -> Expression -> Expression -> Int -> LLVMT m ()
memcpy to from bytes align = do
  to' <- bitcast to (Ptr $ I 8)
  from' <- bitcast from (Ptr $ I 8)
  callFn_ _memcpy [to', from', bytes]
  -- callFn_ _memcpy [to', from', bytes, i32 align, integer (I 1) 0]

  -- label_ "Start"
  -- noop <- icmp Eq bytes $ integer size 0
  -- br noop "Done" "Loop"
  --
  -- label "Loop"
  -- i <- phi [(integer size 0, "Start"), (placeholder "inext" size, "Loop")]
  -- v <- load =<< getelementptr from' [i]
  -- store v =<< getelementptr to' [i]
  -- inext <- as "inext" =<< inc i
  -- done <- icmp Eq inext bytes
  -- br done "Done" "Loop"
  --
  -- label "Done"

callFn :: Monad m => (Name, Parameter Type, [Type], FunctionAttributes) -> [Expression] -> LLVMT m Expression
callFn (name, result, args, _) = call $ GlobalReference name $ Function (parameter result) args

callFn_ :: Monad m => (Name, Parameter Type, [Type], FunctionAttributes) -> [Expression] -> LLVMT m ()
callFn_ (name, result, args, _) = call_ $ GlobalReference name $ Function (parameter result) args

-- * Generated Functions

data GeneratedFn = GeneratedApply ApplyFn
  deriving (Eq, Ord)

data ApplyFn = ApplyFn ApplyType Representation [Representation]
  deriving (Eq, Ord)

data ApplyType = ApplyUnknown | ApplySlow
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
generatedName (GeneratedApply (ApplyFn ApplySlow resultType argTypes)) =
  rtName . pack $ "slow" ++ map repCode (resultType : argTypes)

untag :: Monad m => Expression -> Type -> LLVMT m Expression
untag p ty = do
  bits <- ptrtoint p size
  untagged <- andOp bits untagMask
  inttoptr untagged ty

generatedType :: GeneratedFn -> Type
generatedType (GeneratedApply (ApplyFn ApplySlow resultType _)) = functionType resultType [BoxRep, BoxRep]
generatedType (GeneratedApply (ApplyFn ApplyUnknown resultType argTypes)) = functionType resultType argTypes

generateFunction :: GeneratedFn -> Global
generateFunction = evalLLVM . \case
  toGen@(GeneratedApply (ApplyFn ApplySlow resultType argTypes)) ->
    let fn = Arg "fn" box
        args = Arg "args" (Ptr box)
     in singleFunctionDefinition (generatedName toGen) LinkOnceODR [args, fn] $ do
          fp <- untag (argReference fn) $ functionType resultType argTypes
          args <- for [0..length argTypes-1] $ \i -> do
            let argType = argTypes !! i
            parg <- flip bitcast (Ptr $ repType argType) =<< getelementptr (argReference args) [i64 i]
            load parg
          ret =<< call fp args
  toGen@(GeneratedApply (ApplyFn ApplyUnknown resultType argTypes)) ->
    let fn = Arg "fn" box
        args = generatedArgs argTypes
        argsType = Struct $ map repType argTypes
     in singleFunctionDefinition (generatedName toGen) LinkOnceODR (args ++ [fn]) $ do
          toMask <- ptrtoint (argReference fn) size
          -- tag <- andOp toMask tagMask
          -- isTrivial <- icmp Eq tag (integer size $ length args)
          -- br isTrivial "Trivial" "Nontrivial"

          -- label "Trivial"
          -- fp <- flip inttoptr (functionType resultType argTypes) =<< andOp toMask untagMask
          -- jump "Fast"

          -- label "Nontrivial"
          -- isClosure <- icmp Eq tag $ integer size 0
          -- br isClosure "Closure" "Function"

          -- label "Function"
          -- ret $ undef (repType resultType) -- TODO

          -- label "Closure"
          -- Tag bits were 0 so no need to mask
          pclosure <- bitcast (argReference fn) (Ptr closure)
          pfn <- load =<< getelementptr pclosure [i64 0, i32 0]
          napplied <- load =<< getelementptr pclosure [i64 0, i32 2]
          notPartial <- icmp Eq napplied $ integer arity 0
          br notPartial "Raw" "Partial"

          label "Raw"
          raw <- bitcast pfn (functionType resultType argTypes)

          -- label_ "Fast"
          -- pfast <- phi [(fp, "Trivial"), (raw, "Raw")]
          ret =<< call raw (map argReference args)

          label "Partial"
          papplied <- getelementptr pclosure [i64 0, i32 4]
          -- ntotal <- flip zext (I 64) =<< addOp napplied (integer arity $ length args)

          -- Allocate space for argument stack
          bytes <- flip zext size =<< load =<< getelementptr pclosure [i64 0, i32 3]
          ptotal <- flip bitcast (Ptr box) =<< heapAlloc bytes

          -- Push previously applied arguments
          memcpy ptotal papplied bytes argAlign

          -- Push new arguments
          pnew <- flip inttoptr (Ptr argsType) =<< addOp bytes =<< ptrtoint ptotal size
          for_ [0..length args - 1] $ \i ->
            store (argReference $ args !! i) =<< getelementptr pnew [i64 0, i32 i]

          -- Make the call
          let tyTarget = Function (repType resultType) [Ptr box]
          target <- bitcast pfn (Ptr tyTarget)
          ret =<< call target [ptotal]
