module Glucose.Codegen.LLVM.Execution where

import Control.Lens
import Control.Lens.Utils (modifies)
import Control.Monad (join)
import Control.Monad.State.Strict
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import Data.Text (Text, pack)
import Data.Traversable (for)
import Glucose.Codegen.LLVM.Closure
import Glucose.Codegen.LLVM.DSL
import Glucose.Codegen.LLVM.Name
import Glucose.Codegen.LLVM.RT
import Glucose.Codegen.LLVM.Types
import Glucose.VarGen (variables)

-- * Pointer tagging

{- | Can a pointer be tagged with the given value? -}
canTag :: Int -> Bool
canTag n = n > 0 && n < alignment

{- | Tag a pointer-typed expression with a number. Has no effect if the alignment is insufficient. -}
tagged :: Int -> Expression -> Expression
tagged n p | canTag n = inttoptr' (addOp' (integer size n) (ptrtoint' p size)) (typeOf p)
tagged _ p = p

untag :: Monad m => Expression -> LLVMT m (Expression, Expression)
untag p = do
  pInt <- ptrtoint p size
  untagged <- andOp pInt (integer size (-alignment))
  tag <- trunc pInt (I 4)
  pure (tag, untagged)


-- * Misc?

{- | Can we pass a tagged function with the given argument types around as a value with the given type? -}
canTagFunction :: [Type] -> Type -> Bool
canTagFunction (length -> nargs) ty = canTag nargs && nargs == length (argTypes ty)

asType :: Monad m => Expression -> Type -> LLVMT m Expression
asType expr@(GlobalReference _ (Ptr (Function _ args))) ty | valueType ty == box && canTagFunction args ty =
  pure $ bitcast' (tagged (length args) expr) box
asType expr@(typeOf -> Ptr (Function _ args)) ty | valueType ty == box =
  buildClosure (integer arity $ length args) expr []
asType expr ty = bitcast expr $ valueType ty


-- * Generated Functions

data Requisite = RequiredConstant Text Expression | GeneratedApply ApplyFn
  deriving (Eq)

instance Ord Requisite where
  RequiredConstant a _ <= RequiredConstant b _ = a <= b
  GeneratedApply a <= GeneratedApply b = a <= b
  RequiredConstant{} <= GeneratedApply{} = True
  GeneratedApply{} <= RequiredConstant{} = False

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

generatedName :: Requisite -> Name
generatedName (RequiredConstant name _) = builtinName name
generatedName (GeneratedApply (ApplyFn ApplyUnknown resultType argTypes)) =
  rtName . pack $ "uapply" ++ map repCode (resultType : argTypes)
generatedName (GeneratedApply (ApplyFn ApplySlow resultType argTypes)) =
  rtName . pack $ "slow" ++ map repCode (resultType : argTypes)

type Codegen = State (Map.Map Requisite Global)

runCodegen :: Codegen [Global] -> [Global]
runCodegen a = let (defs, generated) = runState a Map.empty in defs ++ Map.elems generated

generated :: Requisite -> Codegen Expression
generated fn = globalRef <$> liftM2 fromMaybe (modifies (Map.insert fn) =<< generateFunction fn) (gets $ Map.lookup fn)

generatedType :: Requisite -> Type
generatedType (RequiredConstant _ value) = typeOf value
generatedType (GeneratedApply (ApplyFn ApplySlow resultType _)) = functionType resultType [BoxRep, BoxRep]
generatedType (GeneratedApply (ApplyFn ApplyUnknown resultType argTypes)) = functionType resultType argTypes

errorPartialFunctionApplication :: Requisite
errorPartialFunctionApplication = RequiredConstant "errorPartialFunctionApplication" $
  string "Cannot partially apply a function pointer! Should have received a closure.\n\0"

generateFunction :: Requisite -> Codegen Global
generateFunction = \case
  RequiredConstant name value -> singleVariableDefinition (builtinName name) LinkOnceODR Unnamed (pure value) (Alignment 0)
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
     in singleFunctionDefinition (generatedName toGen) LinkOnceODR (args ++ [fn]) $ do
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
              delegateRef <- lift . generated . GeneratedApply . ApplyFn ApplyUnknown resultType $ drop n argTypes
              ret =<< call delegateRef (map argReference (drop n args) ++ [frest])

          label "PartialFunction"
          abort =<< lift (generated errorPartialFunctionApplication)
          ret . undef $ repType resultType
          -- TODO: this doesn't work because we don't have the slow function pointer
          -- ret =<< buildClosure tag fp (map argReference args)

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

          ret =<< applyClosure "FullClosure" "" resultType argTypes pclosure

          when (length args > 1) $ do
            label "OverClosure"
            let apply n = mkName . pack $ "ClosureApply" ++ show n
            switch nunbound (apply 1) [(integer arity n, apply n) | n <- [2 .. length args - 1]]
            for_ (reverse [1 .. length args - 1]) $ \n -> do
              frest <- applyClosure (apply n) (show n) BoxRep (take n argTypes) pclosure
              delegateRef <- lift . generated . GeneratedApply . ApplyFn ApplyUnknown resultType $ drop n argTypes
              ret =<< call delegateRef (map argReference (drop n args) ++ [frest])

          label "PartialClosure"
          pargs <- getPArgs pclosure
          npargs <- getNPArgs pclosure
          ret =<< extendClosure nunbound pfn (map argReference args) pargs nboxed npargs unboxedArgSize
          -- ret $ undef (repType resultType) -- TODO

applyClosure :: Monad m => Name -> String -> Representation -> [Representation] -> Expression -> LLVMT m Expression
applyClosure startLabel suffix resultType argTypes pclosure = do
  let suffixed name = mkName . pack $ name ++ suffix
  let (boxed, unboxed, unboxedType) = splitArgs $ map repType argTypes
  let args = generatedArgs argTypes

  label startLabel
  pfn <- load =<< getFunction pclosure
  nboxed <- load =<< getPArgCount pclosure
  unboxedArgSize <- load =<< getNPArgBytes pclosure

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
  label_ $ suffixed "PushBoxed"
  iboxed <- phi [(integer arity 0, startLabel), (placeholder "iboxed'" arity, suffixed "PushBoxed")]
  from <- getelementptr pboxed [i64 0, iboxed]
  to <- getelementptr pargs [i64 0, iboxed]
  join $ store <$> load from <*> pure to
  iboxed' <- as "iboxed'" =<< inc iboxed
  done <- icmp Eq iboxed' nboxed
  br done (suffixed "PushedBoxed") (suffixed "PushBoxed")
  label $ suffixed "PushedBoxed"

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
  call target [pargs]
