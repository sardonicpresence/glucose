module Glucose.Codegen.LLVM.Closure where

import Control.Lens
import Control.Monad (join)
import Data.List (findIndices)
import Glucose.Codegen.LLVM.DSL
import Glucose.Codegen.LLVM.RT
import Glucose.Codegen.LLVM.Types

getFunction, getUnbound, getPArgCount, getNPArgBytes, getPArgs, getNPArgs :: Monad m => Expression -> LLVMT m Expression
getFunction = flip getelementptr [i64 0, i32 0]
getUnbound = flip getelementptr [i64 0, i32 1]
getPArgCount = flip getelementptr [i64 0, i32 2]
getNPArgBytes = flip getelementptr [i64 0, i32 3]
getPArgs = flip getelementptr [i64 0, i32 4]
getNPArgs = flip getelementptr [i64 0, i32 5]

getPArg, getNPArg :: Monad m => Expression -> Int -> LLVMT m Expression
getPArg p i = getelementptr p [i64 0, i32 4, integer arity i]
getNPArg p i = getelementptr p [i64 0, i32 5, i32 i]

{- | Builds a closure with the given original arity, function & bound arguments.
   The given function must have the slow calling-convention i.e. take a single pointer
   to an array of boxes containing pointer arguments, followed by a packed struct
   containing non-pointer arguments.
 -}
buildClosure :: Monad m => Expression -> Expression -> [Expression] -> LLVMT m Expression
buildClosure narity f args = do
  let (boxed, unboxed, Packed unboxedTypes) = splitArgs $ map typeOf args
  let tyClosure = closureType (length boxed) unboxedTypes
  comment $ "Build closure applying " ++ show (length unboxed) ++ " unboxed and "
         ++ show (length boxed) ++ " boxed arguments to " ++ show f ++ " with arity " ++ show narity
  pclosure <- heapAllocType tyClosure
  -- slow <- load =<< flip getelementptr [i64 (-1)] =<< bitcast f (Ptr fn)
  join $ store <$> bitcast f (Ptr fn) <*> getFunction pclosure
  nunbound <- flip subOp (integer arity $ length args) =<< zext narity arity
  store nunbound =<< getUnbound pclosure
  store (integer arity $ length boxed) =<< getPArgCount pclosure
  store (sizeOf argsize $ Packed unboxedTypes) =<< getNPArgBytes pclosure
  ifor_ (map (args !!) boxed) $ \i arg -> store arg =<< getPArg pclosure i
  ifor_ (map (args !!) unboxed) $ \i arg -> store arg =<< getNPArg pclosure i
  bitcast pclosure box

extendClosure :: Monad m => Expression -> Expression -> [Expression] -> Expression -> Expression -> Expression -> Expression -> LLVMT m Expression
extendClosure narity f args pargs pargCount npargs npargBytes = do
  let (boxed, unboxed, Packed newUnboxedTypes) = splitArgs $ map typeOf args
  let tyClosure = closureType (length boxed) []
  -- comment $ "Build closure applying " ++ show (length unboxed) ++ " unboxed and "
  --        ++ show (length boxed) ++ " boxed arguments to " ++ show f ++ " with arity " ++ show narity
  pargBytes <- mulOp (sizeOf size box) =<< zext pargCount size
  closureBytes <- join $ addOp <$> zext npargBytes size <*> addOp pargBytes (sizeOf size tyClosure)
  pclosure <- flip bitcast (Ptr tyClosure) =<< heapAlloc closureBytes
  join $ store <$> bitcast f (Ptr fn) <*> getFunction pclosure
  nunbound <- flip subOp (integer arity $ length args) =<< zext narity arity
  store nunbound =<< getUnbound pclosure
  nboxed <- addOp pargCount (integer arity $ length boxed)
  store nboxed =<< getPArgCount pclosure
  join $ store <$> addOp npargBytes (sizeOf argsize $ Packed newUnboxedTypes) <*> getNPArgBytes pclosure
  (\to -> memcpy to pargs pargBytes 16) =<< getPArgs pclosure
  ifor_ (map (args !!) boxed) $ \i arg -> do
    i' <- addOp pargCount $ integer arity i
    store arg =<< getelementptr pclosure [i64 0, i32 4, i']
  pnp <- getelementptr pclosure [i64 0, i32 4, nboxed]
  memcpy pnp npargs npargBytes 16
  npargBytes' <- zext npargBytes size
  pnp' <- flip inttoptr (Ptr $ Packed newUnboxedTypes) =<< addOp npargBytes' =<< ptrtoint pnp size
  ifor_ (map (args !!) unboxed) $ \i arg -> store arg =<< getelementptr pnp' [i64 0, i32 i]
  bitcast pclosure box

splitArgs :: [Type] -> ([Int], [Int], Type)
splitArgs types = (boxed, fst unboxed, Packed $ snd unboxed) where
  boxed = findIndices isBoxed types
  unboxed = unzip $ itoListOf (folded . filtered (not . isBoxed)) types
  isBoxed = (BoxRep ==) . typeRep
