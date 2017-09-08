module Glucose.Codegen.LLVM.RT
(
  functionDeclarations, heapAlloc, heapAllocType, heapAllocN,
  splitArgs, tagged,
  Representation(..), GeneratedFn(..), ApplyFn(..), ApplyType(..),
  generateFunction, generatedName, generatedType,
) where

import Control.Lens
import Data.Foldable
import Data.List
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
_heapAlloc = (rtName "heapAlloc", Parameter box True True (Just 16), [size], FunctionAttributes (Just 0))

_memcpy :: (Name, Parameter Type, [Type], FunctionAttributes)
_memcpy = ("llvm.memcpy.p0i8.p0i8.i64", Parameter Void False False Nothing, [Ptr (I 8), Ptr (I 8), I 64, I 32, I 1], FunctionAttributes Nothing)
-- _memcpy = ("memcpy", Parameter Void False False, [Ptr (I 8), Ptr (I 8), size], FunctionAttributes Nothing)

_assume :: (Name, Parameter Type, [Type], FunctionAttributes)
_assume = ("llvm.assume", Parameter Void False False Nothing, [I 1], FunctionAttributes Nothing)

functionDeclarations :: [Global]
functionDeclarations = map (uncurry4 FunctionDeclaration) [ _assume, _memcpy, _heapAlloc ]

heapAllocType :: Monad m => Type -> LLVMT m Expression
heapAllocType ty = do
    bytes <- sizeOf size ty
    ptr <- heapAlloc bytes
    bitcast ptr (Ptr ty)

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
  -- callFn_ _memcpy [to', from', bytes]
  callFn_ _memcpy [to', from', bytes, i32 align, integer (I 1) 0]

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
callFn (name, result, args, _) = call $ GlobalReference name . Ptr $ Function (parameter result) args

callFn_ :: Monad m => (Name, Parameter Type, [Type], FunctionAttributes) -> [Expression] -> LLVMT m ()
callFn_ (name, result, args, _) = call_ $ GlobalReference name . Ptr $ Function (parameter result) args

splitArgs :: [Type] -> ([Int], [Int], Type)
splitArgs types = (boxed, fst unboxed, Packed $ snd unboxed) where
  boxed = findIndices isBoxed types
  unboxed = unzip $ itoListOf (folded . filtered (not . isBoxed)) types
  isBoxed = (BoxRep ==) . typeRep


-- * Generated Functions

newtype GeneratedFn = GeneratedApply ApplyFn
  deriving (Eq, Ord)

data ApplyFn = ApplyFn ApplyType Representation [Representation]
  deriving (Eq, Ord)

data ApplyType = ApplyUnknown | ApplySlow
  deriving (Eq, Ord)

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

tagged :: Monad m => Expression -> Int -> Type -> LLVMT m Expression
tagged p tag ty | tag > 0 && tag < 16 = do
  comment $ "Tag " ++ show p ++ " with " ++ show tag
  pInt <- ptrtoint p size
  tagged <- orOp pInt (integer size tag)
  inttoptr tagged ty
tagged _ tag _ = error $ "Cannot tag a pointer with the value " ++ show tag

untag :: Monad m => Expression -> LLVMT m (Expression, Expression)
untag p = do
  pInt <- ptrtoint p size
  untagged <- andOp pInt untagMask
  tag <- trunc pInt (I 4)
  pure (tag, untagged)

generatedType :: GeneratedFn -> Type
generatedType (GeneratedApply (ApplyFn ApplySlow resultType _)) = functionType resultType [BoxRep, BoxRep]
generatedType (GeneratedApply (ApplyFn ApplyUnknown resultType argTypes)) = functionType resultType argTypes

generateFunction :: GeneratedFn -> Global
generateFunction = evalLLVM . \case
  -- Are these always required? Only for public functions?
  toGen@(GeneratedApply (ApplyFn ApplySlow resultType argTypes)) ->
    let fn = Arg "fn" box
        args = Arg "args" (Ptr box)
     in singleFunctionDefinition (generatedName toGen) LinkOnceODR [args, fn] $ do
          untagged <- snd <$> untag (argReference fn)
          fp <- inttoptr untagged $ functionType resultType argTypes
          args <- for [0..length argTypes-1] $ \i -> do
            let argType = argTypes !! i
            parg <- flip bitcast (Ptr $ repType argType) =<< getelementptr (argReference args) [i64 i]
            load parg
          ret =<< call fp args
  toGen@(GeneratedApply (ApplyFn ApplyUnknown resultType argTypes)) ->
    let fn = Arg "fn" box
        args = generatedArgs argTypes
        (boxed, unboxed, argsType) = splitArgs $ map repType argTypes
     in singleFunctionDefinition (generatedName toGen) LinkOnceODR (args ++ [fn]) $ do
          (tag, untagged) <- untag (argReference fn)
          isTrivial <- icmp Eq tag (integer (I 4) $ length args)
          br isTrivial "Trivial" "Nontrivial"

          label "Trivial"
          fp <- inttoptr untagged $ functionType resultType argTypes
          jump "Fast"

          label "Nontrivial"
          isClosure <- icmp Eq tag $ integer size 0
          br isClosure "Closure" "Function"

          label "Function"
          ret $ undef (repType resultType) -- TODO

          label "Closure"
          pclosure <- inttoptr untagged (Ptr closure) -- TODO: any better to cast from fn direct?
          pfn <- load =<< getelementptr pclosure [i64 0, i32 0]
          nunbound <- load =<< getelementptr pclosure [i64 0, i32 1]
          nboxed <- load =<< getelementptr pclosure [i64 0, i32 2]
          unboxedBytes <- flip zext size =<< load =<< getelementptr pclosure [i64 0, i32 3]
          pboxed <- getelementptr pclosure [i64 0, i32 4]

          noBoxed <- icmp Eq nboxed $ integer arity 0
          noUnboxed <- icmp Eq unboxedBytes $ integer size 0
          notPartial <- andOp noBoxed noUnboxed
          br notPartial "Raw" "Partial"

          label "Raw"
          raw <- bitcast pfn (functionType resultType argTypes)

          label_ "Fast"
          pfast <- phi [(fp, "Trivial"), (raw, "Raw")]
          ret =<< call pfast (map argReference args)

          label "Partial"
          -- ntotal <- flip zext (I 64) =<< addOp napplied (integer arity $ length args)

          -- Allocate space for argument stack
          nboxed' <- addOp nboxed $ integer arity (length boxed)
          boxBytes <- sizeOf size box
          boxedBytes <- mulOp boxBytes =<< zext nboxed' size
          boundBytes <- addOp boxedBytes unboxedBytes
          newBytes <- sizeOf size argsType
          bytes <- addOp boundBytes newBytes
          pargs <- flip bitcast (Ptr box) =<< heapAlloc bytes
          -- ptotal <- flip bitcast (Ptr box) =<< alloca (I 8) bytes

          -- Push bound boxed arguments
          label_ "PushBoxed"
          iboxed <- phi [(integer arity 0, "Partial"), (placeholder "iboxed'" arity, "PushBoxed")]
          from <- getelementptr pboxed [i64 0, iboxed]
          to <- getelementptr pargs [iboxed]
          flip store to =<< load from
          iboxed' <- as "iboxed'" =<< inc iboxed
          done <- icmp Eq iboxed' nboxed
          br done "PushedBoxed" "PushBoxed"
          label "PushedBoxed"

          -- Push new boxed arguments
          for_ (zip [0..] $ map (args !!) boxed) $ \(i, arg) ->
            store (argReference arg) =<< getelementptr pargs . pure =<< addOp iboxed' (integer arity i)

          -- Push bound unboxed arguments
          punboxed <- getelementptr pboxed [i64 0, nboxed]
          punboxed' <- getelementptr pargs [nboxed']
          memcpy punboxed punboxed' unboxedBytes argAlign

          -- Push new unboxed arguments
          pnewUnboxed <- flip inttoptr (Ptr argsType) =<< addOp unboxedBytes =<< ptrtoint punboxed' size
          for_ (zip [0..] $ map (args !!) unboxed) $ \(i, arg) ->
            store (argReference arg) =<< getelementptr pnewUnboxed [i64 0, i32 i]

          -- Make the call
          let tyTarget = Function (repType resultType) [Ptr box]
          target <- bitcast pfn (Ptr tyTarget)
          ret =<< call target [pargs]
