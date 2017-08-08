{-# LANGUAGE TemplateHaskell #-}
module LLVM.DSL where

import LLVM.AST
import LLVM.Name

import Control.Lens hiding (assign)
import Control.Lens.TH ()
import Control.Monad.Identity
import Control.Monad.State hiding (state)
import Data.Maybe

-- * Monad transformer

data DSL = DSL { _globals :: [Global]
               , _blocks :: [BasicBlock]
               , _maybeLabel :: Maybe Name
               , _statements :: [Statement]
               , _lastVar :: Int }

makeLenses ''DSL

type LLVMT m a = StateT DSL m a

type LLVM a = LLVMT Identity a

runLLVM :: LLVM a -> (a, DSL)
runLLVM = runIdentity . runLLVMT

runLLVMT :: LLVMT m a -> m (a, DSL)
runLLVMT m = runStateT m $ DSL [] [] Nothing [] 0

evalLLVMT :: Monad m => LLVMT m a -> m a
evalLLVMT = fmap fst . runLLVMT

evalLLVM :: LLVM a -> a
evalLLVM = fst . runLLVM

execLLVMT :: Monad m => LLVMT m () -> m [Global]
execLLVMT m = do
  DSL globals blocks label statements _ <- snd <$> runLLVMT m
  unless (null blocks && isNothing label && null statements) $ error "incomplete function definition"
  pure globals

mapLLVMT :: (m (a, DSL) -> n (b, DSL)) -> LLVMT m a -> LLVMT n b
mapLLVMT = mapStateT

-- * Declarations/definitions

functionDefinition :: Monad m => Name -> Linkage -> [Arg] -> LLVMT m () -> m [Global]
functionDefinition name linkage args def = do
  DSL defs blocks label statements _ <- snd <$> runLLVMT def
  unless (null statements && isNothing label) $ error "function definition must end in a terminator"
  pure $ defs ++ [FunctionDefinition name linkage args blocks]

singleFunctionDefinition :: Monad m => Name -> Linkage -> [Arg] -> LLVMT m () -> m Global
singleFunctionDefinition name linkage args def = do
  defs <- functionDefinition name linkage args def
  case defs of
    [def] -> pure def
    _ -> error "function definition cannot contain other globals"

defineFunction :: Monad m => Name -> Linkage -> [Arg] -> LLVMT m () -> LLVMT m Expression
defineFunction name linkage args definition = do
  defs <- lift $ functionDefinition name linkage args definition
  globals <>= defs
  pure $ GlobalReference name (typeOf $ last defs)

variableDefinition :: Monad m => Name -> Linkage -> LLVMT m Expression -> m [Global]
variableDefinition name linkage expr = do
  (value, DSL defs blocks label statements _) <- runLLVMT expr
  unless (null blocks && isNothing label && null statements) $ error "variable definition must consist of a single expression"
  pure $ defs ++ [VariableDefinition name linkage value]

defineVariable :: Monad m => Name -> Linkage -> LLVMT m Expression -> LLVMT m Expression
defineVariable name linkage definition = do
  defs <- lift $ variableDefinition name linkage definition
  globals <>= defs
  pure $ GlobalReference name (typeOf $ last defs)

alias :: Monad m => Name -> Expression -> Type -> LLVMT m ()
alias = ((define .) .) . Alias

define :: Monad m => Global -> LLVMT m ()
define def = globals <>= [def]

placeholder :: Name -> Type -> Expression
placeholder = Placeholder

-- * Statements

call :: Monad m => Expression -> [Expression] -> LLVMT m Expression
call = (assignNew .) . Call

call_ :: Monad m => Expression -> [Expression] -> LLVMT m ()
call_ = (state .) . VoidCall

store :: Monad m => Expression -> Expression -> LLVMT m ()
store = (state .) . Store

load :: Monad m => Expression -> LLVMT m Expression
load = assignNew . Load

bitcast :: Monad m => Expression -> Type -> LLVMT m Expression
bitcast = convert Bitcast

getelementptr :: Monad m => Expression -> [Expression] -> LLVMT m Expression
getelementptr = (assignNew .) . GEP

ptrtoint :: Monad m => Expression -> Type -> LLVMT m Expression
ptrtoint = convert PtrToInt

inttoptr :: Monad m => Expression -> Type -> LLVMT m Expression
inttoptr = convert IntToPtr

trunc :: Monad m => Expression -> Type -> LLVMT m Expression
trunc = convert Trunc

zext :: Monad m => Expression -> Type -> LLVMT m Expression
zext = convert Zext

convert :: Monad m => ConversionOp -> Expression -> Type -> LLVMT m Expression
convert _ a ty | typeOf a == ty = pure a
convert _ a (Custom _ ty) | typeOf a == ty = pure a
convert op a ty = assignNew $ Convert op a ty

alloca :: Monad m => Type -> Expression -> LLVMT m Expression
alloca = (assignNew .) . Alloca

andOp :: Monad m => Expression -> Expression -> LLVMT m Expression
andOp = binaryOp And

orOp :: Monad m => Expression -> Expression -> LLVMT m Expression
orOp = binaryOp Or

xorOp :: Monad m => Expression -> Expression -> LLVMT m Expression
xorOp = binaryOp Xor

addOp :: Monad m => Expression -> Expression -> LLVMT m Expression
addOp = binaryOp Add

subOp :: Monad m => Expression -> Expression -> LLVMT m Expression
subOp = binaryOp Sub

mulOp :: Monad m => Expression -> Expression -> LLVMT m Expression
mulOp = binaryOp Mul

inc :: Monad m => Expression -> LLVMT m Expression
inc a = addOp a $ integer (typeOf a) 1

dec :: Monad m => Expression -> LLVMT m Expression
dec a = subOp a $ integer (typeOf a) 1

icmp :: Monad m => Comparison -> Expression -> Expression -> LLVMT m Expression
icmp = binaryOp . ICmp

binaryOp :: Monad m => BinaryOp -> Expression -> Expression -> LLVMT m Expression
binaryOp = ((assignNew .) .) . BinaryOp

phi :: Monad m => [(Expression, Name)] -> LLVMT m Expression
phi = assignNew . Phi

label :: Monad m => Name -> LLVMT m ()
label name = maybeLabel .= Just name

label_ :: Monad m => Name -> LLVMT m ()
label_ name = do
  jump name
  maybeLabel .= Just name

ret :: Monad m => Expression -> LLVMT m ()
ret = terminator . Return

jump :: Monad m => Name -> LLVMT m ()
jump = terminator . Jump

br :: Monad m => Expression -> Name -> Name -> LLVMT m ()
br = ((terminator .) .) . Branch

unreachable :: Monad m => LLVMT m ()
unreachable = terminator Unreachable

assignNew :: Monad m => Assignment -> LLVMT m Expression
assignNew assignment = do
  name <- newLocal
  assign name assignment
  pure $ LocalReference name $ typeOf assignment

as :: Monad m => Name -> Expression -> LLVMT m Expression
as placeholder ref = do
  statements . mapped . expressions %= \case
    Placeholder var _ | var == placeholder -> ref
    a -> a
  pure ref

assign :: Monad m => Name -> Assignment -> LLVMT m ()
assign name assignment = state $ Assignment name assignment

terminator :: Monad m => Terminator -> LLVMT m ()
terminator term = modify $ \(DSL globals blocks label statements lastVar) ->
    DSL globals (blocks ++ [BasicBlock label statements term]) Nothing [] lastVar

state :: Monad m => Statement -> LLVMT m ()
state statement = statements <>= [statement]

-- * Expressions

zeroinitializer :: Type -> Expression
zeroinitializer = Literal . ZeroInitializer

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

undef :: Type -> Expression
undef = Undefined

-- * Utilities

newLocal :: Monad m => LLVMT m Name
newLocal = do lastVar += 1; uses lastVar localName

sizeOf :: Monad m => Type -> Type -> LLVMT m Expression
sizeOf tySize ty = flip ptrtoint tySize =<< getelementptr (zeroinitializer $ Ptr ty) [i64 1]
