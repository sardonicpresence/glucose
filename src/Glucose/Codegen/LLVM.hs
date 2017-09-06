module Glucose.Codegen.LLVM (codegen, codegenModuleDefinitions, codegenModule, codegenDefinitions, win64, llvmType) where

import Glucose.Codegen.LLVM.NameGen
import Glucose.Codegen.LLVM.RT
import Glucose.Codegen.LLVM.Types

import Control.Comonad
import Control.Monad.Trans (lift)
import Control.Monad.Writer
import Data.Foldable
import Data.Function
import Data.List
import qualified Data.Set as Set
import Data.Text (Text, pack)
import Data.Traversable
import Glucose.Identifier
import Glucose.IR.Checked as IR
import LLVM.AST as LLVM
import LLVM.DSL as LLVM hiding (LLVM)
import LLVM.Name

import Debug.Trace

win64 :: Target
win64 = Target (DataLayout LittleEndian Windows [(LLVM.I 64, 64, Nothing)] [8,16,32,64] (Just 128))
               (Triple "x86_64" "pc" "windows")

-- * Compiler interface

codegen :: Comonad f => IR.Module f -> Text
codegen = pack . show . codegenModule

codegenModuleDefinitions :: Comonad f => IR.Module f -> Text
codegenModuleDefinitions (IR.Module defs) = pack . concatMap show . codegenDefinitions $ map extract defs

-- * Test interface

codegenModule :: Comonad f => IR.Module f -> LLVM.Module
codegenModule (IR.Module []) = LLVM.Module win64 []
codegenModule (IR.Module defs) = LLVM.Module win64 $ amble ++ codegenDefinitions (map extract defs)

codegenDefinitions :: Comonad f => [IR.Definition f] -> [LLVM.Global]
codegenDefinitions = runCodegen . execLLVMT . mapM_ definition

-- * Codegen monad

type LLVM a = LLVMT (NameGenT Codegen) a

type Codegen = Writer (Set.Set GeneratedFn)

runCodegen :: Codegen [LLVM.Global] -> [LLVM.Global]
runCodegen a = let (defs, toGenerate) = runWriter a in map generateFunction (Set.toList toGenerate) ++ defs

-- * Internals

definition :: Comonad f => Definition f -> LLVMT Codegen ()
definition (Definition (nameOf -> name) def) =
  mapLLVMT (withNewScope name) $ case extract def of
    Reference Global to rep _ -> alias name (GlobalReference (nameOf to) $ llvmType rep) (llvmType rep)
    Lambda args expr -> do
      let llvmArgs = map (argument . extract) args
      void $ defineFunction name External llvmArgs (ret =<< expression (extract expr))
    _ -> void $ defineVariable name External $ expression (extract def)
definition (Constructor (nameOf -> name) _ index) =
  void $ defineVariable name External (pure $ i32 index)

expression :: Comonad f => IR.Expression f -> LLVM LLVM.Expression
expression (IR.Literal value) = pure $ literal value
expression (Reference Local (nameOf -> name) ty _) = pure $ LLVM.LocalReference name (llvmType ty)
expression (Reference Global (nameOf -> name) ty _) = case ty of
  CheckedType IR.Function{} -> pure $ LLVM.GlobalReference name (llvmType ty)
  _ -> load $ LLVM.GlobalReference name (Ptr $ llvmType ty)
expression (Lambda (map extract -> args) (extract -> def)) = withNewGlobal $ \name ->
  buildLambda name Private args def
expression (Apply (extract -> f) (extract -> x) _) = do
  let (g, as) = flatten f x
  g' <- expression g
  let (Application result calls partial) = traceShowId $ groupApplication (traceShowId $ repOf g) as
  let results = replicate (length calls - 1) fn ++ [maybe fn (const $ llvmType result) partial]
  fullApplications <- foldlM genCall g' (zip results calls)
  maybe (pure fullApplications) (partialApplication fullApplications) partial

repOf :: Comonad f => IR.Expression f -> IR.Type
repOf (Reference _ _ ty _) = ty
repOf expr = IR.typeOf expr

genCall :: Comonad f => LLVM.Expression -> (LLVM.Type, Call (IR.Expression f)) -> LLVM LLVM.Expression
genCall f (retType, args) = do
  ssaArgs <- traverse (\(ty, arg) -> asArg (llvmType ty) =<< expression arg) args
  case LLVM.typeOf f of
    Ptr LLVM.Function{} -> LLVM.call f ssaArgs
    _ -> do
      fnApply <- getApply ApplyUnknown retType (map (llvmType . fst) args)
      result <- LLVM.call fnApply (ssaArgs ++ [bitcastFunctionRef f])
      asArg retType result

  -- Application rep root calls partial -> maybe full partialApply partial where
  --   full = do
  --     fn <- expression root
  --     foldlM genCall fn $ callsWithTypes rep (IR.typeOf root) calls

-- traced :: (a -> String) -> a -> a
-- traced f a = trace (f a) a

-- logCalls :: [([IR.Expression f], IR.Type, IR.Type)] -> String
-- logCalls = foldMap $ \(args, rep, ty) -> "[" ++ show rep ++ " <as> " ++ show ty ++ "](" ++ intercalate ", " (map show args) ++ ")\n"

argument :: IR.Arg -> LLVM.Arg
argument (IR.Arg name ty) = LLVM.Arg (nameOf name) (llvmType ty)

callsWithTypes :: IR.Type -> IR.Type -> [[IR.Expression f]] -> [([IR.Expression f], IR.Type, IR.Type)]
callsWithTypes rep ty calls = zip3 calls (map fst foo) (map snd foo) where
  foo = scanl (uncurry applicationResult) (rep, ty) calls

buildLambda :: Comonad f => Name -> Linkage -> [IR.Arg] -> IR.Expression f -> LLVM LLVM.Expression
buildLambda name linkage args def = do
  let captured = map argument $ Set.toList (captures def) \\ args
  let fnArgs = captured ++ map argument args
  lambda <- defineFunction name linkage fnArgs $ ret =<< expression def
  if null captured
    then pure lambda
    else do
      slow <- defineWrapper lambda
      buildClosure (length fnArgs) slow $ map argReference captured

defineWrapper :: LLVM.Expression -> LLVM LLVM.Expression
defineWrapper fn@(GlobalReference name (Ptr (LLVM.Function _ argTypes))) = let pargs = LLVM.Arg (mkName "args") (Ptr box) in
  defineFunction (slowName name) External [pargs] $ do
    let (boxed, unboxed, unboxedType) = splitArgs argTypes
    let argsType = Struct [Array (length boxed) box, unboxedType]
    pargs' <- bitcast (argReference pargs) (Ptr argsType)
    argsBoxed <- for [0..length boxed - 1] $ \i -> load =<< getelementptr pargs' [i64 0, i32 0, integer arity i]
    argsUnboxed <- for [0..length unboxed - 1] $ \i -> load =<< getelementptr pargs' [i64 0, i32 1, integer arity i]
    let args = map snd . sortBy (compare `on` fst) $ zip boxed argsBoxed ++ zip unboxed argsUnboxed
    ret =<< call fn args
defineWrapper _ = error "Can only define wrappers for global functions!"

withNewGlobal :: (Name -> LLVM a) -> LLVM a
withNewGlobal f = lift newGlobal >>= \name -> mapLLVMT (lift . withNewScope name) (f name)

buildClosure :: Int -> LLVM.Expression -> [LLVM.Expression] -> LLVM LLVM.Expression
buildClosure narity f args = do
  let (boxed, unboxed, Packed unboxedTypes) = splitArgs $ map LLVM.typeOf args
  let tyClosure = closureType (length boxed) unboxedTypes
  bytes <- sizeOf size tyClosure
  ptr <- heapAlloc bytes
  pclosure <- bitcast ptr (Ptr tyClosure)
  rfn <- bitcast f (Ptr fn)
  -- slow <- load =<< flip getelementptr [i64 (-1)] =<< bitcast f (Ptr fn)
  store rfn =<< getelementptr pclosure [i64 0, i32 0]
  store (integer arity $ narity - length args) =<< getelementptr pclosure [i64 0, i32 1]
  store (integer arity $ length boxed) =<< getelementptr pclosure [i64 0, i32 2]
  unboxedBytes <- sizeOf argsize (Packed unboxedTypes)
  store unboxedBytes =<< getelementptr pclosure [i64 0, i32 3]
  for_ (zip [0..] $ map (args !!) boxed) $ \(i, arg) ->
    store arg =<< getelementptr pclosure [i64 0, i32 4, integer arity i]
  for_ (zip [0..] $ map (args !!) unboxed) $ \(i, arg) ->
    store arg =<< getelementptr pclosure [i64 0, i32 5, integer arity i]
  pure ptr

partialApplication :: LLVM.Expression -> Partial f -> LLVM LLVM.Expression
partialApplication f (Partial n args) = error $ "Partial application of " <> show (length args) <> " arguments to " <> show f <> " leaving arity " <> show n

-- genCall :: Comonad f => LLVM.Expression -> ([IR.Expression f], IR.Type, IR.Type) -> LLVM LLVM.Expression
-- genCall fn (params, llvmType -> rep, llvmType -> ty) = do
--   ssaArgs <- zipWithM asArg (LLVM.argTypes rep) =<< traverse expression params
--   case rep of
--     LLVM.Function retType argTypes ->
--       case LLVM.typeOf fn of
--         Ptr LLVM.Function{} -> LLVM.call fn ssaArgs
--         _ -> do
--           fnApply <- getApply ApplyUnknown retType argTypes
--           result <- LLVM.call fnApply (ssaArgs ++ [bitcastFunctionRef fn])
--           asArg (LLVM.returnType ty) result
--       -- ssaFn <- asFunction fn $ LLVM.Ptr ty
--       -- LLVM.call ssaFn ssaArgs
--     _ -> error $ "Unsupported: " <> show ty -- TODO

getApply :: ApplyType -> LLVM.Type -> [LLVM.Type] -> LLVM LLVM.Expression
getApply _ returnTy tys = do
  let genType = GeneratedApply $ ApplyFn ApplyUnknown (typeRep returnTy) (map typeRep tys)
  tell $ Set.singleton genType
  pure $ LLVM.GlobalReference (generatedName genType) (generatedType genType)

asArg :: LLVM.Type -> LLVM.Expression -> LLVM LLVM.Expression
asArg ty arg | ty == LLVM.typeOf arg = pure arg
asArg ty arg | sameRepresentation ty (LLVM.typeOf arg) = bitcastAndTag arg ty
asArg ty arg | Ptr ty == LLVM.typeOf arg = load arg
asArg ty arg | sameRepresentation (Ptr ty) (LLVM.typeOf arg) = bitcast arg (Ptr ty) >>= load
asArg ty arg | sameRepresentation ty (Ptr $ LLVM.typeOf arg) = do
  bytes <- sizeOf size (LLVM.typeOf arg)
  ptr <- heapAlloc bytes
  box <- bitcast ptr (Ptr $ LLVM.typeOf arg)
  store arg box
  if LLVM.typeOf ptr == ty
    then pure ptr
    else bitcast box ty
asArg ty arg = error $ "Cannot apply expression of type " ++ show (LLVM.typeOf arg) ++ " as argument of type " ++ show ty

bitcastAndTag :: LLVM.Expression -> LLVM.Type -> LLVM LLVM.Expression
bitcastAndTag expr ty | ty == box = case LLVM.typeOf expr of
  Ptr (LLVM.Function _ args) -> buildClosure (length args) expr []
  _ -> bitcast expr ty
bitcastAndTag expr ty = bitcast expr ty

bitcastFunctionRef :: LLVM.Expression -> LLVM.Expression
bitcastFunctionRef a@(LLVM.GlobalReference _ LLVM.Function{}) = ConstConvert LLVM.Bitcast a box
bitcastFunctionRef a = a

slowName :: Name -> Name
slowName (Name n) = Name $ n <> "$$"

repType :: IR.Type -> IR.Type -> IR.Type
repType (CheckedType Polymorphic{}) ty = ty
repType rep _ = rep

applicationResult :: IR.Type -> IR.Type -> [IR.Expression f] -> (IR.Type, IR.Type)
applicationResult = go where
  go rep ty [] = (repType rep ty, ty)
  go rep ty (_:as) = applicationResult (retType $ repType rep ty) (retType ty) as

retType :: IR.Type -> IR.Type
retType (CheckedType (IR.Function _ _ b)) = b
retType _ = error "Non-function does not have a return type!"

literal :: IR.Literal -> LLVM.Expression
literal (IR.IntegerLiteral n) = i32 n
literal (IR.FloatLiteral n) = f64 n

llvmType :: IR.Type -> LLVM.Type
llvmType (CheckedType ty) = case ty of
  Unboxed Integer -> LLVM.I 32
  Unboxed Float -> LLVM.F64
  Boxed{} -> box
  Polymorphic{} -> box
  ADT{} -> LLVM.I 32
  IR.Function UnknownArity _ _ -> fn
  IR.Function (Arity n) (varType . llvmType -> from) (llvmType -> to) -> Ptr $ case to of
    LLVM.Function f as -> if n == 1
      then LLVM.Function box [from]
      else LLVM.Function f (from : as)
    f -> LLVM.Function f [from]

varType :: LLVM.Type -> LLVM.Type -- TODO: repType . typeRep
varType LLVM.Function{} = box
varType a = a

nameOf :: (Bound f a, Comonad f) => a -> Name
nameOf name = case identify name of Identifier n -> mkName n

amble :: [LLVM.Global]
amble = typeDeclarations ++ functionDeclarations
