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

type Codegen = Writer (Set.Set GeneratedFn)

type LLVM a = LLVMT (NameGenT Codegen) a

win64 :: Target
win64 = Target (DataLayout LittleEndian Windows [(LLVM.I 64, 64, Nothing)] [8,16,32,64] (Just 128))
               (Triple "x86_64" "pc" "windows")

codegen :: Comonad f => IR.Module f -> Text
codegen = pack . show . codegenModule

codegenModule :: Comonad f => IR.Module f -> LLVM.Module
codegenModule (IR.Module []) = LLVM.Module win64 []
codegenModule (IR.Module defs) = LLVM.Module win64 $ amble ++ codegenDefinitions (map extract defs)

codegenModuleDefinitions :: Comonad f => IR.Module f -> Text
codegenModuleDefinitions (IR.Module defs) = pack . concatMap show . codegenDefinitions $ map extract defs

codegenDefinitions :: Comonad f => [IR.Definition f] -> [LLVM.Global]
codegenDefinitions = runCodegen . execLLVMT . mapM_ definition

runCodegen :: Codegen [LLVM.Global] -> [LLVM.Global]
runCodegen a = case runWriter a of
  (defs, toGenerate) -> map generateFunction (Set.toList toGenerate) ++ defs

definition :: Comonad f => Definition f -> LLVMT Codegen ()
definition (Definition (extract -> Identifier n) def) = let name = mkName n in
  mapLLVMT (withNewScope name) $ case extract def of
    Reference Global (Identifier to) rep _ -> alias name (GlobalReference (mkName to) $ llvmType rep) (llvmType rep)
    Lambda args expr -> do
      let llvmArgs = map (argument . extract) args
      void $ defineFunction name External llvmArgs (ret =<< expression (extract expr))
    _ -> void $ defineVariable name External $ expression (extract def)
definition (Constructor (extract -> Identifier n) _ index) = let name = mkName n in
  void $ defineVariable name External (pure $ i32 index)

defineWrapper :: LLVM.Expression -> LLVM LLVM.Expression
defineWrapper fn@(GlobalReference name (LLVM.Function _ argTypes)) = let pargs = LLVM.Arg (mkName "args") (Ptr box) in
  defineFunction (slowName name) External [pargs] $ do
    let (boxed, unboxed, unboxedType) = splitArgs argTypes
    let argsType = Struct [Array (length boxed) box, unboxedType]
    pargs' <- bitcast (argReference pargs) (Ptr argsType)
    argsBoxed <- for [0..length boxed - 1] $ \i -> load =<< getelementptr pargs' [i64 0, i32 0, integer arity i]
    argsUnboxed <- for [0..length unboxed - 1] $ \i -> load =<< getelementptr pargs' [i64 0, i32 1, integer arity i]
    let args = map snd . sortBy (compare `on` fst) $ zip boxed argsBoxed ++ zip unboxed argsUnboxed
    ret =<< call fn args
defineWrapper _ = error "Can only define wrappers for global functions!"

slowName :: Name -> Name
slowName (Name n) = Name $ n <> "$$"

argument :: IR.Arg -> LLVM.Arg
argument (IR.Arg (Identifier name) ty) = LLVM.Arg (mkName name) (llvmType ty)

expression :: Comonad f => IR.Expression f -> LLVM LLVM.Expression
expression (IR.Literal value) = pure $ literal value
expression (Reference Local (Identifier (mkName -> name)) rep _) = pure $ LLVM.LocalReference name (llvmType rep)
expression (Reference Global (Identifier (mkName -> name)) rep _) =
  let ref = LLVM.GlobalReference name (llvmType rep)
   in case rep of
        Checked IR.Function{} -> pure ref
        _ -> load ref
expression (Lambda args def) = withNewGlobal $ \name -> buildLambda name Private (map extract args) (extract def)
expression (Apply (extract -> f) (extract -> args) _) = case flattenApply f args of
  Application rep root calls partial -> maybe full partialApply partial where
    full = do
      fn <- expression root
      foldlM genCall fn $ callsWithTypes rep (IR.typeOf root) calls

-- traced :: (a -> String) -> a -> a
-- traced f a = trace (f a) a

-- logCalls :: [([IR.Expression f], IR.Type, IR.Type)] -> String
-- logCalls = foldMap $ \(args, rep, ty) -> "[" ++ show rep ++ " <as> " ++ show ty ++ "](" ++ intercalate ", " (map show args) ++ ")\n"

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

partialApply :: Partial f -> LLVM LLVM.Expression
partialApply _ = undefined -- TODO: partial application

genCall :: Comonad f => LLVM.Expression -> ([IR.Expression f], IR.Type, IR.Type) -> LLVM LLVM.Expression
genCall fn (params, llvmType -> rep, llvmType -> ty) = do
  ssaArgs <- zipWithM asArg (LLVM.argTypes rep) =<< traverse expression params
  case rep of
    LLVM.Function retType argTypes ->
      case LLVM.typeOf fn of
        Ptr LLVM.Function{} -> LLVM.call fn ssaArgs
        _ -> do
          fnApply <- getApply ApplyUnknown retType argTypes
          result <- LLVM.call fnApply (ssaArgs ++ [bitcastFunctionRef fn])
          asArg (LLVM.returnType ty) result
      -- ssaFn <- asFunction fn $ LLVM.Ptr ty
      -- LLVM.call ssaFn ssaArgs
    _ -> error $ "Unsupported: " <> show ty -- TODO

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

repType :: IR.Type -> IR.Type -> IR.Type
repType (Checked Polymorphic{}) ty = ty
repType rep _ = rep

applicationResult :: IR.Type -> IR.Type -> [IR.Expression f] -> (IR.Type, IR.Type)
applicationResult = go where
  go rep ty [] = (repType rep ty, ty)
  go rep ty (_:as) = applicationResult (retType $ repType rep ty) (retType ty) as

retType :: IR.Type -> IR.Type
retType (Checked (IR.Function _ _ b)) = b
retType _ = error "Non-function does not have a return type!"

literal :: IR.Literal -> LLVM.Expression
literal (IR.IntegerLiteral n) = i32 n
literal (IR.FloatLiteral n) = f64 n

llvmType :: IR.Type -> LLVM.Type
llvmType (Checked ty) = case ty of
  Unboxed Integer -> LLVM.I 32
  Unboxed Float -> LLVM.F64
  Boxed{} -> box
  Polymorphic{} -> box
  ADT{} -> LLVM.I 32
  IR.Function UnknownArity _ _ -> fn
  IR.Function (Arity n) (varType . llvmType -> from) (llvmType -> to) -> case to of
    LLVM.Function f as -> if n == 1
      then LLVM.Function box [from]
      else LLVM.Function f (from : as)
    f -> LLVM.Function f [from]

varType :: LLVM.Type -> LLVM.Type -- TODO: repType . typeRep
varType LLVM.Function{} = box
varType a = a

amble :: [LLVM.Global]
amble = typeDeclarations ++ functionDeclarations
