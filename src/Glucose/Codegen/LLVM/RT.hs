module Glucose.Codegen.LLVM.RT (functionDeclarations, heapAlloc, heapAllocN) where

import Glucose.Codegen.LLVM.Types
import LLVM.AST
import LLVM.DSL
import LLVM.Name

_heapAlloc :: (Name, Type)
_heapAlloc = (rtName "heapAlloc", Function box [I 64])

functionDeclarations :: [Global]
functionDeclarations = map (uncurry FunctionDeclaration) [ _heapAlloc ]

heapAlloc :: Monad m => Expression -> LLVMT m Expression
heapAlloc bytes = callFn _heapAlloc [bytes]

heapAllocN :: Monad m => Int -> LLVMT m Expression
heapAllocN = heapAlloc . i64

callFn :: Monad m => (Name, Type) -> [Expression] -> LLVMT m Expression
callFn (name, ty) = call $ GlobalReference name ty
