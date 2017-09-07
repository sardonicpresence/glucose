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

amble :: [LLVM.Global]
amble = typeDeclarations ++ functionDeclarations

definition :: Comonad f => Definition f -> LLVMT Codegen ()
definition (Definition (nameOf -> name) def) =
  mapLLVMT (withNewScope name) $ case extract def of
    Reference Global to ty -> alias name (GlobalReference (nameOf to) $ llvmType ty) (llvmType ty)
    Lambda args expr -> do
      let llvmArgs = map (argument . extract) args
      void $ defineFunction name External llvmArgs (ret =<< expression (extract expr))
    _ -> void $ defineVariable name External $ expression (extract def)
definition (Constructor (nameOf -> name) _ index) =
  void $ defineVariable name External (pure $ i32 index)

expression :: Comonad f => IR.Expression f -> LLVM LLVM.Expression
expression (IR.Literal value) = pure $ literal value
expression (Reference Local (nameOf -> name) ty) = pure $ LLVM.LocalReference name (llvmType ty)
expression (Reference Global (nameOf -> name) ty) = case ty of
  CheckedType IR.Function{} -> pure $ LLVM.GlobalReference name (llvmType ty)
  _ -> load $ LLVM.GlobalReference name (Ptr $ llvmType ty)
expression (Lambda (map extract -> args) (extract -> def)) = withNewGlobal $ \name ->
  buildLambda name Private args def
expression (Apply (extract -> f) (extract -> x) _) = do
  let (root, args) = flatten f x
  let (Application result calls partial) = groupApplication (IR.typeOf root) args
  let returnTypes = replicate (length calls - 1) box ++ [maybe box (const $ llvmType result) partial]
  root' <- expression root
  fullApplications <- foldlM (uncurry . fullApplication) root' $ zip returnTypes calls
  maybe (pure fullApplications) (partialApplication fullApplications) partial

argument :: IR.Arg -> LLVM.Arg
argument (IR.Arg name ty) = LLVM.Arg (nameOf name) (llvmType ty)

literal :: IR.Literal -> LLVM.Expression
literal (IR.IntegerLiteral n) = i32 n
literal (IR.FloatLiteral n) = f64 n

-- * Function application

fullApplication :: Comonad f => LLVM.Expression -> LLVM.Type -> Call (IR.Expression f) -> LLVM LLVM.Expression
fullApplication f retType args = do
  let prepareArg (ty, arg) = coerce (llvmType ty) =<< expression arg
  preparedArgs <- traverse prepareArg args
  case LLVM.typeOf f of
    Ptr LLVM.Function{} -> LLVM.call f preparedArgs
    _ -> do
      let argTypes = map (llvmType . fst) args
      fnApply <- getApply ApplyUnknown retType argTypes
      result <- LLVM.call fnApply $ preparedArgs ++ [bitcastFunctionRef f]
      coerce retType result

partialApplication :: LLVM.Expression -> Partial f -> LLVM LLVM.Expression
partialApplication f (Partial n args) = -- TODO
  error $ "Partial application of " <> show (length args) <> " arguments to " <> show f <> " leaving arity " <> show n

getApply :: ApplyType -> LLVM.Type -> [LLVM.Type] -> LLVM LLVM.Expression
getApply applyType returnType argTypes = do
  let genType = GeneratedApply $ ApplyFn applyType (typeRep returnType) (map typeRep argTypes)
  tell $ Set.singleton genType
  pure $ LLVM.GlobalReference (generatedName genType) (generatedType genType)

-- * Lambdas & Closures

buildLambda :: Comonad f => Name -> Linkage -> [IR.Arg] -> IR.Expression f -> LLVM LLVM.Expression
buildLambda name linkage args def = do
  let captured = map argument $ Set.toList (captures def) \\ args
  let fnArgs = captured ++ map argument args
  lambda <- defineFunction name linkage fnArgs $ ret =<< expression def
  if null captured
    then pure lambda
    else do
      slow <- defineSlowWrapper lambda
      buildClosure (length fnArgs) slow $ map argReference captured

defineSlowWrapper :: LLVM.Expression -> LLVM LLVM.Expression
defineSlowWrapper fn@(GlobalReference name (Ptr (LLVM.Function _ argTypes))) = do
  let pargs = LLVM.Arg (mkName "args") (Ptr box)
  defineFunction (slowName name) External [pargs] $ do -- TODO: Could this be private?
    let (boxed, unboxed, unboxedType) = splitArgs argTypes
    let argsType = Struct [Array (length boxed) box, unboxedType]
    pargs' <- bitcast (argReference pargs) (Ptr argsType)
    argsBoxed <- for [0..length boxed - 1] $ \i -> load =<< getelementptr pargs' [i64 0, i32 0, integer arity i]
    argsUnboxed <- for [0..length unboxed - 1] $ \i -> load =<< getelementptr pargs' [i64 0, i32 1, integer arity i]
    let args = map snd . sortBy (compare `on` fst) $ zip boxed argsBoxed ++ zip unboxed argsUnboxed
    ret =<< call fn args
defineSlowWrapper _ = error "Can only define wrappers for global functions!"

buildClosure :: Int -> LLVM.Expression -> [LLVM.Expression] -> LLVM LLVM.Expression
buildClosure narity f args = do
  comment "Build closure"
  let (boxed, unboxed, Packed unboxedTypes) = splitArgs $ map LLVM.typeOf args
  let tyClosure = closureType (length boxed) unboxedTypes
  pclosure <- heapAllocType tyClosure
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
  bitcast pclosure box

-- * Casts & Conversions

coerce :: LLVM.Type -> LLVM.Expression -> LLVM LLVM.Expression
coerce (valueType -> ty) arg = case LLVM.typeOf arg of
  tyArg | ty == tyArg -> pure arg
        | sameRepresentation ty tyArg -> bitcastAndTag arg ty
        | Ptr ty == tyArg -> load arg
        | sameRepresentation (Ptr ty) tyArg -> bitcast arg (Ptr ty) >>= load
        | sameRepresentation ty (Ptr tyArg) -> boxValue arg >>= flip bitcast ty -- TODO: avoid bitcast if unnecessary?
        | otherwise -> error $ "Cannot apply expression of type " ++ show tyArg ++ " as argument of type " ++ show ty

boxValue :: LLVM.Expression -> LLVM LLVM.Expression
boxValue expr = do
  comment "Box value"
  let ty = LLVM.typeOf expr
  box <- heapAllocType ty
  store expr box
  pure box

bitcastAndTag :: LLVM.Expression -> LLVM.Type -> LLVM LLVM.Expression
bitcastAndTag expr ty | ty == box = case LLVM.typeOf expr of
  Ptr (LLVM.Function _ args) -> buildClosure (length args) expr []
  _ -> bitcast expr ty
bitcastAndTag expr ty = bitcast expr ty

bitcastFunctionRef :: LLVM.Expression -> LLVM.Expression
bitcastFunctionRef a@(LLVM.GlobalReference _ LLVM.Function{}) = ConstConvert LLVM.Bitcast a box
bitcastFunctionRef a = a

llvmType :: IR.Type -> LLVM.Type
llvmType (Type (Checked ty)) = case ty of
  Unboxed Integer -> LLVM.I 32
  Unboxed Float -> LLVM.F64
  Boxed{} -> box
  Polymorphic{} -> box
  ADT{} -> LLVM.I 32
  IR.Function UnknownArity _ _ -> fn
  IR.Function (Arity n) (valueType . llvmType -> from) (llvmType -> to) -> Ptr $ case to of
    Ptr (LLVM.Function f as) -> if n == 1
      then LLVM.Function box [from]
      else LLVM.Function f (from : as)
    f -> LLVM.Function f [from]

valueType :: LLVM.Type -> LLVM.Type
valueType = repType . typeRep

-- * Utilities

withNewGlobal :: (Name -> LLVM a) -> LLVM a
withNewGlobal f = lift newGlobal >>= \name -> mapLLVMT (lift . withNewScope name) (f name)

slowName :: Name -> Name
slowName (Name n) = Name $ n <> "$$"

nameOf :: (Bound f a, Comonad f) => a -> Name
nameOf name = case identify name of Identifier n -> mkName n
