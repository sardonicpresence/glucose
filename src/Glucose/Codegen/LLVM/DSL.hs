module Glucose.Codegen.LLVM.DSL
(
  module LLVM.AST,
  module LLVM.DSL,
  module Glucose.Codegen.LLVM.DSL
)
where

import LLVM.AST
import LLVM.DSL hiding (functionDefinition, singleFunctionDefinition, defineFunction, defineVariable, call, call_)
import qualified LLVM.DSL as DSL
import LLVM.Name

functionDefinition :: Monad m => Name -> Linkage -> [Arg] -> LLVMT m () -> m [Global]
functionDefinition name linkage args = DSL.functionDefinition name linkage callingConvention args functionAttributes

singleFunctionDefinition :: Monad m => Name -> Linkage -> [Arg] -> LLVMT m () -> m Global
singleFunctionDefinition name linkage args = DSL.singleFunctionDefinition name linkage callingConvention args functionAttributes

defineFunction :: Monad m => Name -> Linkage -> [Arg] -> LLVMT m () -> LLVMT m Expression
defineFunction name linkage args = DSL.defineFunction name linkage callingConvention args functionAttributes

defineVariable :: Monad m => Name -> Linkage -> LLVMT m Expression -> LLVMT m Expression
defineVariable name linkage expr = DSL.defineVariable name linkage Unnamed expr (Alignment 0)

-- | Standard function attributes.
functionAttributes :: FunctionAttributes
functionAttributes = FunctionAttributes Unnamed [] [0] (Alignment 0)

call :: Monad m => Expression -> [Expression] -> LLVMT m Expression
call = DSL.call callingConvention

call_ :: Monad m => Expression -> [Expression] -> LLVMT m ()
call_ = DSL.call_ callingConvention

callingConvention :: CallingConvention
callingConvention = FastCC
