module LLVM.DSL where

import LLVM.AST
import LLVM.Name

import Control.Arrow ((&&&))
import Control.Monad.RWS (RWST, evalRWST, mapRWST, lift, tell)
import qualified Control.Monad.RWS as RWS

type DSL = ([Global], [Statement])

type LLVMT m a = RWST () DSL Int m a

evalLLVMT :: Monad m => LLVMT m a -> m (a, ([Global], [Statement]))
evalLLVMT t = evalRWST t () 1

mapLLVMT :: Monad m => (m a -> m a) -> LLVMT m a -> LLVMT m a
mapLLVMT f = mapRWST $ \p -> p >>= \(a, s, w) -> f (pure a) >>= \b -> pure (b, s, w)

-- * Declarations/definitions

functionDefinition :: Monad m => Name -> Linkage -> [Arg] -> LLVMT m Expression -> LLVMT m Expression
functionDefinition name linkage args def = do
  (result, (globals, statements)) <- lift $ evalLLVMT def
  mapM_ define globals
  define $ FunctionDefinition name linkage args statements result
  pure $ GlobalReference name $ Function (typeOf result) (map typeOf args)

define :: Monad m => Global -> LLVMT m ()
define def = tell ([def], [])

-- * Statements

call :: Monad m => Expression -> [Expression] -> LLVMT m Expression
call f as = assign $ \n -> (returnType (typeOf f), Call n f as)

store :: Monad m => Expression -> Expression -> LLVMT m ()
store from to = state $ Store from to

load :: Monad m => Expression -> LLVMT m Expression
load from = case typeOf from of
  Ptr ty -> assign $ \n -> (ty, Load n ty from)
  ty -> error $ "cannot load from an expression of non-pointer type '" ++ show ty ++ "'"

bitcast :: Monad m => Expression -> Type -> LLVMT m Expression
bitcast from to = assign $ \n -> (to, Bitcast n to from)

getElementPtr :: Monad m => Expression -> [Expression] -> LLVMT m Expression
getElementPtr p indices = assign $ \n -> (Ptr ty, GEP n p indices)
  where ty = foldl dereference (typeOf p) indices
        dereference ty index = case ty of
          Custom _ ty -> dereference ty index
          Ptr ty -> ty
          Array _ ty -> ty
          Struct tys -> tys !! case index of
            Literal (IntegerLiteral _ _ a) -> fromInteger a
            Literal a -> error $ "cannot use non-integral type as an index: " ++ show (typeOf a)
            _ -> error "cannot use a non-constant expression to index into a structure type"
          _ -> error $ "cannot getElementPtr of non-aggregate value type: " ++ show ty

state :: Monad m => Statement -> LLVMT m ()
state statement = tell ([], [statement])

assign :: Monad m => (Int -> (Type, Statement)) -> LLVMT m Expression
assign f = do
  n <- newLocal
  let (ty, statement) = f n
  state statement
  pure $ LocalReference (localName n) ty

-- * Expressions

i32 :: Int -> Expression
i32 = integer (I 32)

i64 :: Integral a => a -> Expression
i64 = integer (I 64)

f64 :: Double -> Expression
f64 = float F64

integer :: Integral a => Type -> a -> Expression
integer (I bits) value = Literal $ IntegerLiteral Nothing bits (fromIntegral value)
integer (Custom name ty) value = case integer ty value of
  Literal (IntegerLiteral _ bits value) -> Literal $ IntegerLiteral (Just name) bits value
integer ty _ = error $ "not a valid type for an integer literal: " ++ show ty

float ::  Real a => Type -> a -> Expression
float F64 value = Literal $ FloatLiteral Nothing (fromRational $ toRational value)
float (Custom name ty) value = case float ty value of
  Literal (FloatLiteral _ value) -> Literal $ FloatLiteral (Just name) value
float ty _ = error $ "not a valid type for a floating-point literal: " ++ show ty

-- * Utilities

newLocal :: Monad m => LLVMT m Int
newLocal = RWS.state $ id &&& (+1)
