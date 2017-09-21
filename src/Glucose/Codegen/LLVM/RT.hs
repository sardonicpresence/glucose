module Glucose.Codegen.LLVM.RT
(
  defineFunction, alias, asType, buildClosure,
  functionDeclarations, attributeGroups, heapAlloc, heapAllocType, heapAllocN,
  functionAttributes, splitArgs, tagged, untag,
  Representation(..), GeneratedFn(..), ApplyFn(..), ApplyType(..),
  generateFunction, generatedName, generatedType,
) where

import Control.Lens
import Control.Monad
import Data.Foldable
import Data.List
import Data.Text (pack)
import Data.Traversable
import Glucose.Codegen.LLVM.DSL hiding (defineFunction, alias)
import qualified Glucose.Codegen.LLVM.DSL as DSL
import Glucose.Codegen.LLVM.Types
import Glucose.VarGen
import LLVM.Name

-- * Code generation internals

alignment :: Int
alignment = 16

defineFunction :: Monad m => Name -> Linkage -> [Arg] -> LLVMT m () -> LLVMT m Expression
defineFunction name linkage args def = do
  fn <- DSL.defineFunction name linkage args functionAttributes def
  DSL.alias (taggedName name) linkage Named (tagged (length args) fn) (deref $ typeOf fn)

alias :: Monad m => Name -> Linkage -> UnnamedAddr -> Expression -> Type -> LLVMT m Expression
alias name linkage addr value@(GlobalReference ref refTy@Function{}) ty = do
  void $ DSL.alias name linkage addr value ty
  DSL.alias (taggedName name) linkage addr (GlobalReference (taggedName ref) refTy) ty
alias name linkage addr value ty = DSL.alias name linkage addr value ty

{- | Tag a pointer-typed expression with a number. Has no effect if the alignment is insufficient. -}
tagged :: Int -> Expression -> Expression
tagged n p | canTag n = inttoptr' (addOp' (integer size n) (ptrtoint' p size)) (typeOf p)
tagged _ p = p

{- | Can a pointer be tagged with the given value? -}
canTag :: Int -> Bool
canTag n = n > 0 && n < alignment

asType :: Monad m => Expression -> Type -> LLVMT m Expression
asType (GlobalReference name refTy@(Ptr (Function _ args))) ty | ty == box && canTag (length args) =
  pure $ bitcast' (GlobalReference (taggedName name) refTy) box
-- TODO: if/when does the below case occur?
asType expr@(typeOf -> Ptr (Function _ args)) ty | ty == box = buildClosure (integer arity $ length args) expr []
asType expr ty = bitcast expr ty

getFunction, getUnbound, getPArgCount, getNPArgBytes :: Monad m => Expression -> LLVMT m Expression
getFunction = flip getelementptr [i64 0, i32 0]
getUnbound = flip getelementptr [i64 0, i32 1]
getPArgCount = flip getelementptr [i64 0, i32 2]
getNPArgBytes = flip getelementptr [i64 0, i32 3]
getPArgs = flip getelementptr [i64 0, i32 4]

getPArg, getNPArg :: Monad m => Expression -> Int -> LLVMT m Expression
getPArg p i = getelementptr p [i64 0, i32 4, integer arity i]
getNPArg p i = getelementptr p [i64 0, i32 5, i32 i]

{- | Builds a closure with the given original arity, function & bound arguments.
   The given function must have the slow calling-convention i.e. take a single pointer
   to an array of boxes containing pointer arguments, followed by a packed struct
   containing non-pointer arguments.
 -}
buildClosure :: Monad m => Expression -> Expression -> [Expression] -> LLVMT m Expression
buildClosure narity f args = do
  let (boxed, unboxed, Packed unboxedTypes) = splitArgs $ map typeOf args
  let tyClosure = closureType (length boxed) unboxedTypes
  comment $ "Build closure applying " ++ show (length unboxed) ++ " unboxed and "
         ++ show (length boxed) ++ " boxed arguments to " ++ show f ++ " with arity " ++ show narity
  pclosure <- heapAllocType tyClosure
  -- slow <- load =<< flip getelementptr [i64 (-1)] =<< bitcast f (Ptr fn)
  join $ store <$> bitcast f (Ptr fn) <*> getFunction pclosure
  nunbound <- flip subOp (integer arity $ length args) =<< zext narity arity
  store nunbound =<< getUnbound pclosure
  store (integer arity $ length boxed) =<< getPArgCount pclosure
  store (sizeOf argsize $ Packed unboxedTypes) =<< getNPArgBytes pclosure
  ifor_ (map (args !!) boxed) $ \i arg -> store arg =<< getPArg pclosure i
  ifor_ (map (args !!) unboxed) $ \i arg -> store arg =<< getNPArg pclosure i
  bitcast pclosure box


-- * Built-in runtime functions

_heapAlloc :: Global
_heapAlloc = FunctionDeclaration (Name "$heapAlloc") External callingConvention result args attrs where
  result = Parameter ["nonnull", "noalias"] (Alignment 16) box
  args = [Parameter [] (Alignment 0) size]
  attrs = FunctionAttributes Unnamed ["allocsize(0)"] [0] (Alignment alignment)

_memcpy :: Global
_memcpy = FunctionDeclaration (Name "llvm.memcpy.p0i8.p0i8.i64") External callingConvention result args attrs where
  result = pure Void
  args = map pure [Ptr (I 8), Ptr (I 8), I 64, I 32, I 1]
  attrs = noAttributes Unnamed

functionDeclarations :: [Global]
functionDeclarations = [_memcpy, _heapAlloc]

attributeGroups :: [Global]
attributeGroups = [AttributeGroup 0 ["nounwind", "align=" ++ show alignment]]

heapAllocType :: Monad m => Type -> LLVMT m Expression
heapAllocType ty = flip bitcast (Ptr ty) =<< heapAlloc (sizeOf size ty)

heapAlloc :: Monad m => Expression -> LLVMT m Expression
heapAlloc bytes = call (globalRef _heapAlloc) [bytes]

heapAllocN :: Monad m => Int -> LLVMT m Expression
heapAllocN = heapAlloc . integer size

memcpy :: Monad m => Expression -> Expression -> Expression -> Int -> LLVMT m ()
memcpy to from bytes align = do
  to' <- bitcast to (Ptr $ I 8)
  from' <- bitcast from (Ptr $ I 8)
  call_ (globalRef _memcpy) [to', from', bytes, i32 align, i1 False]

-- * Utilities

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

-- tagged :: Monad m => Expression -> Int -> Type -> LLVMT m Expression
-- tagged p tag ty | tag > 0 && tag < alignment = do
--   comment $ "Tag " ++ show p ++ " with " ++ show tag
--   tagged <- orOp (integer size tag) =<< ptrtoint p size
--   inttoptr tagged ty
-- tagged _ tag _ = error $ "Cannot tag a pointer with the value " ++ show tag

untag :: Monad m => Expression -> LLVMT m (Expression, Expression)
untag p = do
  pInt <- ptrtoint p size
  untagged <- andOp pInt (integer size (-alignment))
  tag <- trunc pInt (I 4)
  pure (tag, untagged)

-- | Standard function attributes.
functionAttributes :: FunctionAttributes
functionAttributes = FunctionAttributes Unnamed [] [0] (Alignment 0)

generatedType :: GeneratedFn -> Type
generatedType (GeneratedApply (ApplyFn ApplySlow resultType _)) = functionType resultType [BoxRep, BoxRep]
generatedType (GeneratedApply (ApplyFn ApplyUnknown resultType argTypes)) = functionType resultType argTypes

generateFunction :: GeneratedFn -> Global
generateFunction = evalLLVM . \case
  -- Are these always required? Only for public functions?
  toGen@(GeneratedApply (ApplyFn ApplySlow resultType argTypes)) ->
    let fn = Arg "fn" box
        args = Arg "args" (Ptr box)
     in singleFunctionDefinition (generatedName toGen) LinkOnceODR [args, fn] functionAttributes $ do
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
        (boxed, unboxed, unboxedType) = splitArgs $ map repType argTypes
     in singleFunctionDefinition (generatedName toGen) LinkOnceODR (args ++ [fn]) functionAttributes $ do
          (tag, untagged) <- untag (argReference fn)
          fp <- inttoptr untagged $ functionType resultType argTypes
          isClosure <- icmp Eq tag $ i4 0
          br isClosure "Closure" "Function"

          label "Function"
          when (length args > 1) $ do
            isOver <- icmp Ult tag $ i4 (length args)
            br_ isOver "OverFunction"
          isPartial <- icmp Ugt tag $ i4 (length args)
          br_ isPartial "PartialFunction"

          comment "Fully applied function"
          ret =<< call fp (map argReference args)

          when (length args > 1) $ do
            label "OverFunction"
            let apply n = mkName . pack $ "Apply" ++ show n
            switch tag (apply 1) [(i4 n, apply n) | n <- [2 .. length args - 1]]
            for_ (reverse [1 .. length args - 1]) $ \n -> do
              label $ apply n
              fp <- inttoptr untagged $ functionType resultType $ take n argTypes
              frest <- call fp (map argReference $ take n args)
              let delegate = GeneratedApply . ApplyFn ApplyUnknown resultType $ drop n argTypes
              let delegateRef = GlobalReference (generatedName delegate) (generatedType delegate)
              ret =<< call delegateRef (map argReference (drop n args) ++ [frest])

          label "PartialFunction"
          ret =<< buildClosure tag fp (map argReference args)

          label "Closure"
          pclosure <- inttoptr untagged (Ptr closure) -- TODO: any better to cast from fn direct?
          pfn <- load =<< getFunction pclosure
          nboxed <- load =<< getPArgCount pclosure
          unboxedArgSize <- load =<< getNPArgBytes pclosure
          hasBound <- icmp Ne (integer argsize 0) =<< addOp unboxedArgSize =<< zext nboxed argsize
          br hasBound "Bound" "NoBound"

          label "NoBound"
          raw <- bitcast pfn (functionType resultType argTypes)
          ret =<< call raw (map argReference args)

          label "Bound"
          nunbound <- load =<< getUnbound pclosure
          when (length args > 1) $ do
            isOver <- icmp Ult nunbound $ i4 (length args)
            br_ isOver "OverClosure"
          isPartial' <- icmp Ugt nunbound $ i4 (length args)
          br isPartial' "PartialClosure" "FullClosure"

          label "FullClosure"

          comment "Allocate space for argument stack"
          nboxed' <- flip zext size =<< addOp nboxed (integer arity $ length boxed)
          boxedBytes' <- mulOp nboxed' (sizeOf size box)
          unboxedBytes <- zext unboxedArgSize size
          unboxedBytes' <- addOp (sizeOf size unboxedType) unboxedBytes
          argBytes <- addOp boxedBytes' unboxedBytes'
          -- pargs <- flip bitcast (Ptr $ Array 0 box) =<< alloca (I 8) argBytes
          pargs <- flip bitcast (Ptr $ Array 0 box) =<< heapAlloc argBytes

          comment "Push bound boxed arguments"
          pboxed <- getPArgs pclosure
          label_ "PushBoxed"
          iboxed <- phi [(integer arity 0, "FullClosure"), (placeholder "iboxed'" arity, "PushBoxed")]
          from <- getelementptr pboxed [i64 0, iboxed]
          to <- getelementptr pargs [i64 0, iboxed]
          join $ store <$> load from <*> pure to
          iboxed' <- as "iboxed'" =<< inc iboxed
          done <- icmp Eq iboxed' nboxed
          br done "PushedBoxed" "PushBoxed"
          label "PushedBoxed"

          comment "Push new boxed arguments"
          ifor_ (map (args !!) boxed) $ \i arg -> do
            index <- addOp nboxed $ integer arity i
            store (argReference arg) =<< getelementptr pargs [i64 0, index]

          comment "Push bound unboxed arguments"
          punboxed <- getelementptr pboxed [i64 0, nboxed]
          punboxed' <- getelementptr pargs [i64 0, nboxed']
          -- TODO: consider using something simpler than memcpy - there will be few bytes
          memcpy punboxed punboxed' unboxedBytes alignment

          comment "Push new unboxed arguments"
          pnewUnboxed <- flip inttoptr (Ptr unboxedType) =<< addOp unboxedBytes =<< ptrtoint punboxed' size
          ifor_ (map (args !!) unboxed) $ \i arg ->
            store (argReference arg) =<< getelementptr pnewUnboxed [i64 0, i32 i]

          comment "Make the call"
          let tyTarget = Function (repType resultType) [Ptr $ Array 0 box]
          target <- flip inttoptr (Ptr tyTarget) =<< snd <$> untag pfn -- TODO: shouldn't be required
          -- target <- bitcast pfn (Ptr tyTarget)
          ret =<< call target [pargs]

          label "OverClosure"
          ret $ undef (repType resultType) -- TODO

          label "PartialClosure"
          ret $ undef (repType resultType) -- TODO
