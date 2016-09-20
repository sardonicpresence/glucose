module Glucose.Codegen.LLVM (codegen) where

import Control.Comonad
import Glucose.Identifier
import Glucose.IR
import qualified LLVM.AST as LLVM

codegen :: Module -> LLVM.Module
codegen (Module defs) = LLVM.Module $ map (definition . extract) defs

definition :: Definition -> LLVM.Global
definition (Definition (extract -> Identifier name) (extract -> def)) = let n = LLVM.mkName name in case def of
  Literal value -> LLVM.VariableDefinition n $ LLVM.Literal $ constant value
  Reference Local (Identifier name) ty -> LLVM.VariableDefinition n $ LLVM.LocalReference (LLVM.mkName name) (llvmType ty)
  Reference Global (Identifier name) ty -> LLVM.Alias n (LLVM.mkName name) (llvmType ty)

constant :: Literal -> LLVM.Constant
constant (IntegerLiteral n) = LLVM.I32 n
constant (FloatLiteral n) = LLVM.F64 n

llvmType :: Type -> LLVM.Type
llvmType Integer = LLVM.TI32
llvmType Float = LLVM.TF64
