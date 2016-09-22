module Glucose.Codegen.LLVM (codegen) where

import Control.Comonad
import Glucose.Codegen.LLVM.LocalScope
import Glucose.Identifier
import Glucose.IR
import qualified LLVM.AST as LLVM

codegen :: Module -> LLVM.Module
codegen (Module defs) = LLVM.Module $ map (definition . extract) defs

definition :: Definition -> LLVM.Global
definition (Definition (extract -> Identifier name) (extract -> def)) = let n = LLVM.mkName name in case def of
  Reference Global (Identifier name) ty -> LLVM.Alias n (LLVM.mkName name) (llvmType ty)
  _ -> case withNewScope $ expression def of
    ([], [], value) -> LLVM.VariableDefinition n value
    (args, assigns, value) -> LLVM.FunctionDefinition n args assigns value

argument :: Arg -> LLVM.Arg
argument (Arg (Identifier name) ty) = LLVM.Arg (LLVM.mkName name) (llvmType ty)

expression :: Expression -> LocalScope ([LLVM.Arg], [LLVM.Assignment], LLVM.Expression)
expression (Literal value) = pure ([], [], LLVM.Literal $ constant value)
expression (Reference Local (Identifier name) ty) = pure ([], [], LLVM.LocalReference (LLVM.mkName name) (llvmType ty))
expression (Reference Global (Identifier name) ty) =
  case ty of
    Function{} -> pure ([], [], ref)
    _ -> do
      n <- newLocal
      pure ([], [LLVM.Load n (llvmType ty) ref], LLVM.LocalReference (LLVM.localName n) (llvmType ty))
  where ref = LLVM.GlobalReference (LLVM.mkName name) (llvmType ty)
expression (Lambda args def) = case withNewScope . expression $ extract def of
  (moreArgs, assigns, value) -> pure (map (argument . extract) args ++ moreArgs, assigns, value)
expression (Apply expr arg) = do
  (_, asExpr, f) <- expression $ extract expr
  (_, asArg, a) <- expression $ extract arg
  n <- newLocal
  let fn = case f of
             LLVM.LocalReference fn _ -> fn
             LLVM.GlobalReference fn _ -> fn
             _ -> error "Cannot call a non-function!"
  pure ([], asExpr ++ asArg ++ [LLVM.Call n LLVM.Box fn [a]], LLVM.LocalReference (LLVM.localName n) LLVM.Box)

constant :: Literal -> LLVM.Constant
constant (IntegerLiteral n) = LLVM.I32 n
constant (FloatLiteral n) = LLVM.F64 n

llvmType :: Type -> LLVM.Type
llvmType Integer = LLVM.TI32
llvmType Float = LLVM.TF64
llvmType Bound{} = LLVM.Box
llvmType Free{} = error "Free variable left for code-generator!"
llvmType Function{} = LLVM.Box
