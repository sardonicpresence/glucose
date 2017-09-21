module Glucose.Codegen.LLVM.DSL
(
  module LLVM.AST,
  module LLVM.DSL,
  functionDefinition, singleFunctionDefinition, defineFunction, call, call_,
  callingConvention
)
where

import LLVM.AST
import LLVM.DSL hiding (functionDefinition, singleFunctionDefinition, defineFunction, call, call_)
import qualified LLVM.DSL as DSL
import LLVM.Name

functionDefinition :: Monad m => Name -> Linkage -> [Arg] -> FunctionAttributes -> LLVMT m () -> m [Global]
functionDefinition name linkage = DSL.functionDefinition name linkage callingConvention

singleFunctionDefinition :: Monad m => Name -> Linkage -> [Arg] -> FunctionAttributes -> LLVMT m () -> m Global
singleFunctionDefinition name linkage = DSL.singleFunctionDefinition name linkage callingConvention

defineFunction :: Monad m => Name -> Linkage -> [Arg] -> FunctionAttributes -> LLVMT m () -> LLVMT m Expression
defineFunction name linkage = DSL.defineFunction name linkage callingConvention

call :: Monad m => Expression -> [Expression] -> LLVMT m Expression
call = DSL.call callingConvention

call_ :: Monad m => Expression -> [Expression] -> LLVMT m ()
call_ = DSL.call_ callingConvention

callingConvention :: CallingConvention
callingConvention = FastCC
