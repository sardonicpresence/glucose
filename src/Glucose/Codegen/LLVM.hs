module Glucose.Codegen.LLVM (codegen, codegenModuleDefinitions, codegenModule, codegenDefinitions, llvmType) where

import Glucose.Codegen.LLVM.NameGen
import Glucose.Codegen.LLVM.RT
import Glucose.Codegen.LLVM.Types

import Control.Comonad
import Control.Lens.Operators
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
import Glucose.Codegen.Target
import LLVM.AST as LLVM hiding (Target, nameOf)
import LLVM.DSL as LLVM hiding (LLVM, defineFunction)
import LLVM.Name

-- * Compiler interface

codegen :: Comonad f => Target -> IR.Module f -> Text
codegen target = pack . show . codegenModule target

codegenModuleDefinitions :: Comonad f => IR.Module f -> Text
codegenModuleDefinitions (IR.Module defs) = pack . concatMap show . codegenDefinitions $ map extract defs

-- * Test interface

codegenModule :: Comonad f => Target -> IR.Module f -> LLVM.Module
codegenModule (llvmTarget -> target) = \case
  IR.Module [] -> LLVM.Module target []
  IR.Module defs -> LLVM.Module target $ preamble ++ codegenDefinitions (map extract defs) ++ postamble

codegenDefinitions :: Comonad f => [IR.Definition f] -> [LLVM.Global]
codegenDefinitions = runCodegen . execLLVMT . mapM_ definition

-- * Codegen monad

type LLVM a = LLVMT (NameGenT Codegen) a

type Codegen = Writer (Set.Set GeneratedFn)

runCodegen :: Codegen [LLVM.Global] -> [LLVM.Global]
runCodegen a = let (defs, toGenerate) = runWriter a in defs ++ map generateFunction (Set.toList toGenerate)

-- * Internals

preamble :: [LLVM.Global]
preamble = typeDeclarations

postamble :: [LLVM.Global]
postamble = functionDeclarations ++ attributeGroups

definition :: Comonad f => Definition f -> LLVMT Codegen ()
definition (Definition (nameOf -> name) def) =
  mapLLVMT (withNewScope name) $ case extract def of
    Reference Global to (llvmType -> ty) -> do
      let tyPtr = case ty of Ptr t -> t; _ -> ty
      void $ alias name External Unnamed (GlobalReference (nameOf to) tyPtr) tyPtr
    Lambda args expr -> do
      let llvmArgs = map (argument . extract) args
      let retTy = llvmType $ IR.typeOf (extract expr) & dataType %~ unboxed
      void $ defineFunction name External llvmArgs $
        ret =<< coerce retTy =<< expression (extract expr)
    _ -> void $ defineVariable name External Unnamed $ expression (extract def)
definition (Constructor (nameOf -> name) _ index) =
  void $ defineVariable name External Unnamed (pure $ i32 index)

expression :: Comonad f => IR.Expression f -> LLVM LLVM.Expression
expression (IR.Literal value) = pure $ literal value
expression (Reference Local (nameOf -> name) ty) = pure $ LLVM.LocalReference name (llvmType ty)
expression (Reference Global (nameOf -> name) ty) = case ty of
  CheckedType IR.Function{} -> pure $ LLVM.GlobalReference name (llvmType ty)
  _ -> load $ LLVM.GlobalReference name (Ptr $ llvmType ty)
expression (Lambda (map extract -> args) (extract -> def)) = withNewGlobal $ \name ->
  buildLambda name Private args def
expression (Apply (extract -> f) (extract -> x) retTy) = do
  let (root, args) = flatten f x
  let (Application result calls partial) = groupApplication (IR.typeOf root) args
  let returnTypes = replicate (length calls - 1) box ++ [maybe box (const $ llvmType result) partial]
  root' <- expression root
  fullApplications <- foldlM (uncurry . fullApplication) root' $ zip returnTypes calls
  coerce (llvmType retTy) =<< maybe (pure fullApplications) (partialApplication fullApplications) partial

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
    Ptr LLVM.Function{} -> do
      comment $ "Call function " ++ show f ++ " of known arity " ++ show (length args)
      LLVM.call f preparedArgs
    _ -> do
      comment $ "Call unknown function " ++ show f
      let argTypes = map (llvmType . fst) args
      fnApply <- getApply ApplyUnknown retType argTypes
      result <- LLVM.call fnApply $ preparedArgs ++ [f]
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


-- * Casts & Conversions

coerce :: LLVM.Type -> LLVM.Expression -> LLVM LLVM.Expression
coerce (valueType -> ty) arg = case LLVM.typeOf arg of
  tyArg | sameRepresentation ty tyArg -> arg `asType` ty
        | sameRepresentation (Ptr ty) tyArg -> load =<< arg `asType` Ptr ty
        | sameRepresentation ty (Ptr tyArg) -> flip bitcast ty =<< boxValue arg
        | otherwise -> error $ "Cannot apply expression of type " ++ show tyArg ++ " as argument of type " ++ show ty

boxValue :: LLVM.Expression -> LLVM LLVM.Expression
boxValue expr = do
  comment $ "Box " ++ show expr
  let ty = LLVM.typeOf expr
  box <- heapAllocType ty
  store expr box
  pure box

llvmType :: IR.Type -> LLVM.Type
llvmType (Type (Checked ty)) = case ty of
  Unboxed Integer -> LLVM.I 32
  Unboxed Float -> LLVM.F64
  Boxed{} -> box
  Polymorphic{} -> box
  ADT{} -> LLVM.I 32
  IR.Function UnknownArity _ _ -> box
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
