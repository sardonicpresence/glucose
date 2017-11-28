module Glucose.Codegen.LLVM (codegen, codegenModuleDefinitions, codegenModule, codegenDefinitions, llvmType) where

import Control.Comonad
import Control.Lens.Operators
import Control.Monad.Identity
import Data.Foldable
import Data.Text (Text, pack)
import Glucose.Codegen.LLVM.DSL as LLVM hiding (LLVM, Target, nameOf)
import Glucose.Codegen.LLVM.NameGen
import Glucose.Codegen.LLVM.Types
import Glucose.Codegen.Target
import Glucose.Identifier
import Glucose.IR.Checked as IR
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
  IR.Module defs -> LLVM.Module target $ preamble ++ codegenDefinitions (map extract defs)

codegenDefinitions :: Comonad f => [IR.Definition f] -> [LLVM.Global]
codegenDefinitions = execLLVM . mapM_ definition

-- * Codegen monad

type LLVM a = LLVMT NameGen a

-- * Internals

preamble :: [LLVM.Global]
preamble = typeDeclarations

definition :: Comonad f => Definition f -> LLVMT Identity ()
definition (Definition (nameOf -> name) def) =
  mapLLVMT (withNewScope name) $ case extract def of
    Reference (Global to) (llvmType -> ty) -> do
      let tyPtr = case ty of Ptr t -> t; _ -> ty
      void $ alias name External Unnamed (GlobalReference (nameOf to) tyPtr) tyPtr
    Lambda arg expr -> do
      let llvmArgs = map (argument . extract) [arg]
      let retTy = llvmType $ IR.typeOf (extract expr) & dataType %~ unboxed
      void $ defineFunction name External llvmArgs $
        ret =<< coerce retTy =<< expression (extract expr)
    _ -> void $ defineVariable name External $ expression (extract def)
definition (Constructor (nameOf -> name) _ index) =
  void $ defineVariable name External (pure $ i32 index)

expression :: Comonad f => IR.Expression f -> LLVM LLVM.Expression
expression (IR.Literal value) = pure $ literal value
expression (Reference (Local (nameOf -> name)) ty) = pure $ LLVM.LocalReference name (llvmType ty)
expression (Reference (Global (nameOf -> name)) ty) = case ty of
  CheckedType IR.Function{} -> pure $ LLVM.GlobalReference name (llvmType ty)
  _ -> load $ LLVM.GlobalReference name (Ptr $ llvmType ty)
expression Lambda{} = error "Non-top-level lambda expression in codegen!"
expression (Apply (extract -> f) (extract -> x) retTy) = do
  let (root, args) = flatten f x
  let (Application result calls partial) = groupApplication (IR.typeOf root) args
  let returnTypes = replicate (length calls - 1) box ++ [maybe box (const $ llvmType result) partial]
  root' <- expression root
  fullApplications <- foldlM (uncurry . fullApplication) root' $ zip returnTypes calls
  coerce (llvmType retTy) =<< maybe (pure fullApplications) (partialApplication fullApplications) partial

argument :: IR.Arg -> LLVM.Arg
argument (IR.Arg name ty) = LLVM.Arg (nameOf name) (valueType $ llvmType ty)

literal :: IR.Literal -> LLVM.Expression
literal (IR.IntegerLiteral n) = i32 n
literal (IR.FloatLiteral n) = f64 n

-- * Function application

fullApplication :: Comonad f => LLVM.Expression -> LLVM.Type -> Call (IR.Expression f) -> LLVM LLVM.Expression
fullApplication f _retType args = do
  let prepareArg (ty, arg) = coerce (llvmType ty) =<< expression arg
  preparedArgs <- traverse prepareArg args
  {- coerce retType =<< -} -- No need to coerce return-types yet, without closures
  LLVM.call f preparedArgs

partialApplication = error "Partial application in codegen!"

-- * Casts & Conversions

coerce :: LLVM.Type -> LLVM.Expression -> LLVM LLVM.Expression
coerce ty val = case (typeRep $ LLVM.typeOf val, typeRep ty) of
  (I32Rep, BoxRep) -> inttoptr val ty
  (F64Rep, BoxRep) -> fptoptr val ty
  (BoxRep, I32Rep) -> ptrtoint val ty
  (BoxRep, F64Rep) -> ptrtofp val ty
  (from, to) | from == to -> bitcast val ty
  _ -> error $ "Invalid cast from " ++ show (LLVM.typeOf val) ++ " to " ++ show ty

llvmType :: IR.Type -> LLVM.Type
llvmType (Type (Checked ty)) = case ty of
  Unboxed Integer -> LLVM.I 32
  Unboxed Float -> LLVM.F64
  Boxed{} -> box
  Polymorphic{} -> box
  ADT{} -> LLVM.I 32
  IR.Function UnknownArity from to -> llvmType . Type . Checked $ IR.Function (Arity 1) from to -- TODO: temporarily: box
  IR.Function (Arity n) (valueType . llvmType -> from) (llvmType -> to) -> Ptr $ case to of
    Ptr (LLVM.Function f as) -> if n == 1
      then LLVM.Function to [from]
      else LLVM.Function f (from : as)
    f -> LLVM.Function f [from]

-- * Utilities

nameOf :: (Bound f a, Comonad f) => a -> Name
nameOf name = case identify name of Identifier n -> mkName n
