module Glucose.Codegen.LLVM (codegen, codegenDefinitions, win64, llvmType) where

import Glucose.Codegen.LLVM.NameGen
import Glucose.Codegen.LLVM.RT
import Glucose.Codegen.LLVM.Types

import Control.Comonad
import Control.Monad.Trans (lift)
import Control.Monad.Writer
import Data.Foldable
import Data.List
import qualified Data.Set as Set
import Data.Traversable
import Glucose.Identifier
-- import Glucose.IR as IR
import Glucose.IR.Checked as IR
import LLVM.AST as LLVM
import LLVM.DSL as LLVM hiding (LLVM)
import LLVM.Name

type Codegen = Writer (Set.Set GeneratedFn)

type LLVM a = LLVMT (NameGenT Codegen) a

win64 :: Target
win64 = Target (DataLayout LittleEndian Windows [(LLVM.I 64, 64, Nothing)] [8,16,32,64] (Just 128))
               (Triple "x86_64" "pc" "windows")

codegen :: IR.Module -> LLVM.Module
codegen (IR.Module []) = LLVM.Module win64 []
codegen (IR.Module defs) = LLVM.Module win64 $ amble ++ codegenDefinitions (map extract defs)

codegenDefinitions :: [IR.Definition] -> [LLVM.Global]
codegenDefinitions = runCodegen . execLLVMT . mapM_ definition

runCodegen :: Codegen [LLVM.Global] -> [LLVM.Global]
runCodegen a = case runWriter a of
  (defs, toGenerate) -> map generateFunction (Set.toList toGenerate) ++ defs

definition :: Definition -> LLVMT Codegen ()
definition (Definition (extract -> Identifier n) def) = let name = mkName n in
  mapLLVMT (withNewScope name) $ case extract def of
    Reference Global (Identifier to) rep _ -> alias name (GlobalReference (mkName to) $ llvmType rep) (llvmType rep)
    Lambda args expr -> do
      let llvmArgs = map (argument . extract) args
      void $ defineFunction name External llvmArgs (ret =<< expression (extract expr))
    _ -> void $ defineVariable name External $ expression (extract def)

defineWrapper :: LLVM.Expression -> LLVM LLVM.Expression
defineWrapper fn@(GlobalReference name (LLVM.Function _ argTypes)) = let pargs = LLVM.Arg (mkName "args") (Ptr box) in
  defineFunction (slowName name) External [pargs] $ do
    args <- for [0..length argTypes-1] $ \i -> do
      let argType = argTypes !! i
      parg <- flip bitcast (Ptr argType) =<< getElementPtr (argReference pargs) [i64 i]
      load parg
    ret =<< call fn args

slowName :: Name -> Name
slowName (Name n) = Name $ n <> "$$"

tagFunction :: LLVM.Expression -> LLVM.Expression
tagFunction f = case LLVM.typeOf f of
  Ptr (LLVM.Function _ args) ->
    -- Uses 'add' to simplify with 'sub' in apply functions ('or' and 'and' doesn't appear to)
    ConstConvert IntToPtr (ConstBinaryOp Add (ConstConvert PtrToInt f size) (integer size $ length args)) box
  _ -> f

argument :: IR.Arg -> LLVM.Arg
argument (IR.Arg (Identifier name) ty) = LLVM.Arg (mkName name) (llvmType ty)

expression :: IR.Expression -> LLVM LLVM.Expression
expression (IR.Literal value) = pure $ literal value
expression (Reference Local (Identifier (mkName -> name)) rep _) = pure $ LLVM.LocalReference name (llvmType rep)
expression (Reference Global (Identifier (mkName -> name)) rep _) =
  let ref = LLVM.GlobalReference name (llvmType rep)
   in case rep of
        IR.Function{} -> pure ref
        _ -> load ref
expression (Constructor _ index) = pure $ i32 index
expression (Lambda args def) = withNewGlobal $ \name -> buildLambda name Private (map extract args) (extract def)
expression (Apply (extract -> f) (extract -> args)) = case flattenApply f args of
  Application rep root calls partial -> maybe full partialApply partial where
    full = do
      fn <- expression root
      foldlM genCall fn $ callsWithTypes rep (IR.typeOf root) calls

callsWithTypes :: IR.Type -> IR.Type -> [[IR.Expression]] -> [([IR.Expression], IR.Type, IR.Type)]
callsWithTypes rep ty calls = zip3 calls (map fst foo) (map snd foo) where
  foo = scanl (uncurry applicationResult) (rep, ty) calls

buildLambda :: Name -> Linkage -> [IR.Arg] -> IR.Expression -> LLVM LLVM.Expression
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
  ptr <- heapAllocN $ 8 * (2 + length args)
  pclosure <- bitcast ptr (Ptr closure)
  pfn <- getElementPtr pclosure [i64 0, i32 0]
  rfn <- bitcast f (Ptr fn)
  -- slow <- load =<< flip getElementPtr [i64 (-1)] =<< bitcast f (Ptr fn)
  store rfn pfn
  parity <- getElementPtr pclosure [i64 0, i32 1]
  store (integer arity narity) parity
  pnargs <- getElementPtr pclosure [i64 0, i32 2]
  store (integer arity $ length args) pnargs
  mapM_ (storeArg pclosure) $ zip [0..] args
  pure ptr

  -- untagged <- flip inttoptr box =<< andOp (integer size (-16)) =<< ptrtoint ptr size
  -- pure untagged

  -- tagged <- orOp untagged (integer arity $ length args)
  -- inttoptr tagged box
  where
    storeArg pclosure (i, arg) = do
      parg <- getElementPtr pclosure [i64 0, i32 3, i32 i]
      store arg parg

partialApply :: Partial -> LLVM LLVM.Expression
partialApply _ = undefined -- TODO: partial application

genCall :: LLVM.Expression -> ([IR.Expression], IR.Type, IR.Type) -> LLVM LLVM.Expression
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
getApply applyType returnTy tys = do
  let genType = GeneratedApply $ ApplyFn ApplyUnknown (typeRep returnTy) (map typeRep tys)
  tell $ Set.singleton genType
  pure $ LLVM.GlobalReference (generatedName genType) (generatedType genType)

asArg :: LLVM.Type -> LLVM.Expression -> LLVM LLVM.Expression
asArg ty arg | ty == LLVM.typeOf arg = pure arg
asArg ty arg | sameRepresentation ty (LLVM.typeOf arg) = bitcastAndTag arg ty
asArg ty arg | Ptr ty == LLVM.typeOf arg = load arg
asArg ty arg | sameRepresentation (Ptr ty) (LLVM.typeOf arg) = bitcast arg (Ptr ty) >>= load
asArg ty arg | sameRepresentation ty (Ptr $ LLVM.typeOf arg) = do
  ptr <- heapAllocN 8 -- TODO: use size of type
  box <- bitcast ptr (Ptr $ LLVM.typeOf arg)
  store arg box
  if LLVM.typeOf ptr == ty
    then pure ptr
    else bitcast box ty
asArg ty arg = error $ "Cannot apply expression of type " ++ show (LLVM.typeOf arg) ++ " as argument of type " ++ show ty

bitcastAndTag :: LLVM.Expression -> LLVM.Type -> LLVM LLVM.Expression
bitcastAndTag expr = bitcast $ tagFunction expr

bitcastFunctionRef :: LLVM.Expression -> LLVM.Expression
bitcastFunctionRef a@(LLVM.GlobalReference _ LLVM.Function{}) = ConstConvert LLVM.Bitcast a box
bitcastFunctionRef a = a

repType :: IR.Type -> IR.Type -> IR.Type
repType Bound{} ty = ty
repType rep _ = rep

applicationResult :: IR.Type -> IR.Type -> [IR.Expression] -> (IR.Type, IR.Type)
applicationResult = go where
  go rep ty [] = (repType rep ty, ty)
  go rep ty (_:as) = applicationResult (IR.returnType $ repType rep ty) (IR.returnType ty) as

literal :: IR.Literal -> LLVM.Expression
literal (IR.IntegerLiteral n) = i32 n
literal (IR.FloatLiteral n) = f64 n

llvmType :: IR.Type -> LLVM.Type
llvmType Integer = LLVM.I 32
llvmType Float = LLVM.F64
llvmType Bound{} = box
llvmType Free{} = error "Free variable left for code-generator!"
llvmType (ADT _) = LLVM.I 32
llvmType (IR.Function UnknownArity _ _) = fn
llvmType (IR.Function (Arity n m) (varType . llvmType -> from) (llvmType -> to)) = case to of
  LLVM.Function f as -> if n == m + 1
    then LLVM.Function box [from]
    else LLVM.Function f (from : as)
  f -> LLVM.Function f [from]

varType :: LLVM.Type -> LLVM.Type -- TODO: repType . typeRep
varType LLVM.Function{} = box
varType a = a

amble :: [LLVM.Global]
amble = typeDeclarations ++ functionDeclarations
