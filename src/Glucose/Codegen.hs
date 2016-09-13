module Glucose.Codegen (codegen) where

import Glucose.Identifier
import Glucose.IR
import qualified LLVM.AST as LLVM

codegen :: Module -> LLVM.Module
codegen (Module defs) = LLVM.Module $ map globalVariable defs

globalVariable :: Definition -> LLVM.Global
globalVariable (Definition (Identifier name) value) = LLVM.VariableDefinition (LLVM.mkName name) (expression value)

expression :: Expression -> LLVM.Expression
expression (Literal value) = LLVM.Literal $ constant value

constant :: Literal -> LLVM.Constant
constant (IntegerLiteral n) = LLVM.I32 n
constant (FloatLiteral n) = LLVM.F64 n
