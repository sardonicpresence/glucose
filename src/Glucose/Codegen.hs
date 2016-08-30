module Glucose.Codegen (codegen) where

import Glucose.AST
import qualified LLVM.AST as LLVM

codegen :: Module -> LLVM.Module
codegen (Module defs) = LLVM.Module $ map globalVariable defs

globalVariable :: Definition -> LLVM.Global
globalVariable (Definition (Identifier name) value _) = LLVM.VariableDefinition (LLVM.mkName name) (constant value)

constant :: Literal -> LLVM.Constant
constant (IntegerLiteral n) = LLVM.I32 n
constant (FloatLiteral n) = LLVM.F64 n
