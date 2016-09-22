module LLVM.DSL where

import LLVM.AST
import LLVM.Name

import Control.Lens hiding (assign)
import Control.Monad.Identity
import Control.Monad.State hiding (state)
import Data.Maybe

-- * Monad transformer

data DSL = DSL { _globals :: [Global]
               , _blocks :: [BasicBlock]
               , _maybeLabel :: Maybe Name
               , _statements :: [Statement]
               , _lastVar :: Int }

globals :: Lens' DSL [Global]
globals = lens _globals $ \a b -> a { _globals = b }

blocks :: Lens' DSL [BasicBlock]
blocks = lens _blocks $ \a b -> a { _blocks = b }

maybeLabel :: Lens' DSL (Maybe Name)
maybeLabel = lens _maybeLabel $ \a b -> a { _maybeLabel = b }

statements :: Lens' DSL [Statement]
statements = lens _statements $ \a b -> a { _statements = b }

lastVar :: Lens' DSL Int
lastVar = lens _lastVar $ \a b -> a { _lastVar = b }

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

alias :: Monad m => Name -> Name -> Type -> LLVMT m ()
alias = ((define .) .) . Alias

define :: Monad m => Global -> LLVMT m ()
define def = globals <>= [def]

-- * Statements

call :: Monad m => Expression -> [Expression] -> LLVMT m Expression
call = (assign .) . Call

store :: Monad m => Expression -> Expression -> LLVMT m ()
store = (state .) . Store

load :: Monad m => Expression -> LLVMT m Expression
load = assign . Load

bitcast :: Monad m => Expression -> Type -> LLVMT m Expression
bitcast = (assign .) . Bitcast

getElementPtr :: Monad m => Expression -> [Expression] -> LLVMT m Expression
getElementPtr = (assign .) . GEP

ptrtoint :: Monad m => Expression -> Type -> LLVMT m Expression
ptrtoint = (assign .) . PtrToInt

andOp :: Monad m => Expression -> Expression -> LLVMT m Expression
andOp = binaryOp And

icmp :: Monad m => Comparison -> Expression -> Expression -> LLVMT m Expression
icmp = binaryOp . ICmp

binaryOp :: Monad m => BinaryOp -> Expression -> Expression -> LLVMT m Expression
binaryOp = ((assign .) .) . BinaryOp

label :: Monad m => Name -> LLVMT m ()
label name = maybeLabel .= Just name

ret :: Monad m => Expression -> LLVMT m ()
ret = terminator . Return

br :: Monad m => Expression -> Name -> Name -> LLVMT m ()
br = ((terminator .) .) . Branch

assign :: Monad m => Assignment -> LLVMT m Expression
assign assignment = do
  name <- newLocal
  state $ Assignment name assignment
  pure $ LocalReference name $ typeOf assignment

terminator :: Monad m => Terminator -> LLVMT m ()
terminator term = modify $ \(DSL globals blocks label statements lastVar) ->
    DSL globals (blocks ++ [BasicBlock label statements term]) Nothing [] lastVar

state :: Monad m => Statement -> LLVMT m ()
state statement = statements <>= [statement]

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

newLocal :: Monad m => LLVMT m Name
newLocal = do lastVar += 1; uses lastVar localName
