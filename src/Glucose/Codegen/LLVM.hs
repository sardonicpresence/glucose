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
    Reference Global (Identifier to) ty -> alias name (mkName to) (llvmType ty)
    Lambda args expr -> do
      tell $ Set.singleton $ GeneratedApply $ ApplyFn ApplyUnknown I32Rep [BoxRep] -- TODO: Debug only!
      void $ defineFunction name External (map (argument . extract) args) (ret =<< expression (extract expr))
    _ -> void $ defineVariable name External $ expression (extract def)

argument :: IR.Arg -> LLVM.Arg
argument (IR.Arg (Identifier name) ty) = LLVM.Arg (mkName name) (llvmType ty)

expression :: IR.Expression -> LLVM LLVM.Expression
expression (IR.Literal value) = pure $ literal value
expression (Reference Local (Identifier (mkName -> name)) ty) = pure $ LLVM.LocalReference name (llvmType ty)
expression (Reference Global (Identifier (mkName -> name)) ty) =
  let ref = LLVM.GlobalReference name (llvmType ty)
   in case ty of
        IR.Function{} -> pure ref
        _ -> load ref
expression (Constructor _ index) = pure $ i32 index
expression (Lambda args def) = withNewGlobal $ \name -> buildLambda name Private (map extract args) (extract def)
expression (Apply (extract -> f) (extract -> args)) = case flattenApply f args of
  Application root calls partial -> maybe full partialApply partial where
    full = do
      fn <- expression root
      let tys = tail $ scanl applicationResult (IR.typeOf root) calls
      foldlM genCall fn $ zip calls tys

buildLambda :: Name -> Linkage -> [IR.Arg] -> IR.Expression -> LLVM LLVM.Expression
buildLambda name linkage args def = do
  let captured = map argument $ Set.toList (captures def) \\ args
  let fnArgs = captured ++ map argument args
  lambda <- defineFunction name linkage fnArgs $ ret =<< expression def
  if null captured
    then pure lambda
    else buildClosure lambda $ map argReference captured

withNewGlobal :: (Name -> LLVM a) -> LLVM a
withNewGlobal f = lift newGlobal >>= \name -> mapLLVMT (lift . withNewScope name) (f name)

buildClosure :: LLVM.Expression -> [LLVM.Expression] -> LLVM LLVM.Expression
buildClosure f args = do
  ptr <- heapAllocN $ 64 * (2 + length args)
  pclosure <- bitcast ptr (Ptr closure)
  pfn <- getElementPtr pclosure [i64 0, i32 0]
  rfn <- bitcast f (Ptr fn)
  store rfn pfn
  parity <- getElementPtr pclosure [i64 0, i32 1]
  store (integer arity $ length args) parity
  mapM_ (storeArg pclosure) $ zip [0..] args
  pure ptr
  where
    storeArg pclosure (i, arg) = do
      parg <- getElementPtr pclosure [i64 0, i32 3, i32 i]
      store arg parg

partialApply :: Partial -> LLVM LLVM.Expression
partialApply _ = undefined -- TODO: partial application

genCall :: LLVM.Expression -> ([IR.Expression], IR.Type) -> LLVM LLVM.Expression
genCall fn (params, ty) = do
  ssaArgs <- traverse (fmap asArg . expression) params
  ssaFn <- asFunction fn $ LLVM.Ptr $ llvmType ty
  LLVM.call ssaFn ssaArgs

asArg :: LLVM.Expression -> LLVM.Expression
asArg = bitcastFunctionRef

asFunction :: LLVM.Expression -> LLVM.Type -> LLVM LLVM.Expression
asFunction expr ty = case LLVM.typeOf expr of
  LLVM.Custom _ _ -> bitcast expr ty -- TODO: restrict to Box or Fn
  _ -> pure expr

bitcastFunctionRef :: LLVM.Expression -> LLVM.Expression
bitcastFunctionRef (LLVM.GlobalReference name ty@LLVM.Function{}) = LLVM.BitcastGlobalRef name ty box
bitcastFunctionRef a = a

applicationResult :: IR.Type -> [IR.Expression] -> IR.Type
applicationResult f [] = f
applicationResult f (_:as) = case f of
  IR.Function{} -> applicationResult f as
  _ -> error $ "cannot apply arguments to expression of type: " ++ show f

nameOf :: LLVM.Expression -> Name
nameOf (LLVM.LocalReference n _) = n
nameOf (LLVM.GlobalReference n _) = n
nameOf a = error $ "cannot apply arguments to expression: " ++ show a

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

varType :: LLVM.Type -> LLVM.Type
varType LLVM.Function{} = box
varType a = a

amble :: [LLVM.Global]
amble = typeDeclarations ++ functionDeclarations
