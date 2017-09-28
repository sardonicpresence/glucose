module Glucose.Codegen.LLVM.RT where

import Glucose.Codegen.LLVM.DSL
import Glucose.Codegen.LLVM.Types
import Glucose.Codegen.LLVM.Name

-- * Built-in runtime functions

_abort :: Global
_abort = FunctionDeclaration (builtinName "abort") External callingConvention result args attrs where
  result = pure Void
  args = [Parameter ["nonnull"] (Alignment 0) . Ptr $ Array 0 (I 8)]
  attrs = FunctionAttributes Unnamed ["norecurse", "noreturn"] [0] (Alignment alignment)

_heapAlloc :: Global
_heapAlloc = FunctionDeclaration (builtinName "heapAlloc") External callingConvention result args attrs where
  result = Result ["nonnull", "noalias"] (Alignment 16) box
  args = [Parameter [] (Alignment 0) size]
  attrs = FunctionAttributes Unnamed ["allocsize(0)"] [0] (Alignment alignment)

_memcpy :: Global
_memcpy = FunctionDeclaration (Name "llvm.memcpy.p0i8.p0i8.i64") External callingConvention result args attrs where
  result = pure Void
  args = map pure [Ptr (I 8), Ptr (I 8), I 64, I 32, I 1]
  attrs = noAttributes Unnamed

functionDeclarations :: [Global]
functionDeclarations = [_abort, _memcpy, _heapAlloc]

attributeGroups :: [Global]
attributeGroups = [AttributeGroup 0 ["nounwind", "align=" ++ show alignment]]

abort :: Monad m => Expression -> LLVMT m ()
abort message = call_ (globalRef _abort) [bitcast' message . Ptr $ Array 0 (I 8)]

heapAllocType :: Monad m => Type -> LLVMT m Expression
heapAllocType ty = flip bitcast (Ptr ty) =<< heapAlloc (sizeOf size ty)

heapAlloc :: Monad m => Expression -> LLVMT m Expression
heapAlloc bytes = call (globalRef _heapAlloc) [bytes]

heapAllocN :: Monad m => Int -> LLVMT m Expression
heapAllocN = heapAlloc . integer size

memcpy :: Monad m => Expression -> Expression -> Expression -> Int -> LLVMT m ()
memcpy to from bytes align = do
  to' <- bitcast to (Ptr $ I 8)
  from' <- bitcast from (Ptr $ I 8)
  bytes' <- zext bytes size
  call_ (globalRef _memcpy) [to', from', bytes', i32 align, i1 False]
