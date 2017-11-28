{-# LANGUAGE TemplateHaskell #-}
module LLVM.DSL where

import LLVM.AST
import LLVM.Name

import Control.Lens hiding (assign)
import Control.Lens.TH ()
import Control.Monad.Identity
import Control.Monad.State hiding (state)
import Data.Maybe
import Data.Monoid ((<>))

-- * LLVMT monad transformer

data DSL = DSL { _globals :: [Global]
               , _blocks :: [BasicBlock]
               , _maybeLabel :: Maybe Name
               , _statements :: [Statement]
               , _lastVar :: Int }

makeLenses ''DSL

type LLVMT m a = StateT DSL m a

runLLVMT :: LLVMT m a -> m (a, DSL)
runLLVMT m = runStateT m $ DSL [] [] Nothing [] 0

evalLLVMT :: Monad m => LLVMT m a -> m a
evalLLVMT = fmap fst . runLLVMT

execLLVMT :: Monad m => LLVMT m a -> m [Global]
execLLVMT m = do
  DSL globals blocks label statements _ <- snd <$> runLLVMT m
  unless (null blocks && isNothing label && null statements) $ error "incomplete function definition"
  pure globals

mapLLVMT :: (m (a, DSL) -> n (b, DSL)) -> LLVMT m a -> LLVMT n b
mapLLVMT = mapStateT

-- * LLVM monad

type LLVM a = LLVMT Identity a

runLLVM :: LLVM a -> (a, DSL)
runLLVM = runIdentity . runLLVMT

evalLLVM :: LLVM a -> a
evalLLVM = fst . runLLVM

execLLVM :: LLVM a -> [Global]
execLLVM = runIdentity . execLLVMT

-- * Declarations/definitions

functionDefinition :: Monad m => Name -> Linkage -> CallingConvention -> [Arg] -> FunctionAttributes -> LLVMT m () -> m [Global]
functionDefinition name linkage cc args attrs def = do
  DSL defs blocks label statements _ <- snd <$> runLLVMT def
  unless (null statements && isNothing label) $ error "function definition must end in a terminator"
  pure $ defs ++ [FunctionDefinition name linkage cc (map pure args) attrs (pure blocks)]

singleFunctionDefinition :: Monad m => Name -> Linkage -> CallingConvention -> [Arg] -> FunctionAttributes -> LLVMT m () -> m Global
singleFunctionDefinition name linkage cc args attrs def = single "function definition" =<< functionDefinition name linkage cc args attrs def

defineFunction :: Monad m => Name -> Linkage -> CallingConvention -> [Arg] -> FunctionAttributes -> LLVMT m () -> LLVMT m Expression
defineFunction name linkage cc args attrs definition =
  define =<< lift (functionDefinition name linkage cc args attrs definition)

variableDefinition :: Monad m => Name -> Linkage -> UnnamedAddr -> LLVMT m Expression -> Alignment -> m [Global]
variableDefinition name linkage addr expr align = do
  (value, DSL defs blocks label statements _) <- runLLVMT expr
  unless (null blocks && isNothing label && null statements) $ error "variable definition must consist of a single expression"
  pure $ defs ++ [VariableDefinition name linkage addr value align]

singleVariableDefinition :: Monad m => Name -> Linkage -> UnnamedAddr -> LLVMT m Expression -> Alignment -> m Global
singleVariableDefinition name linkage addr expr align = single "variable definition" =<< variableDefinition name linkage addr expr align

defineVariable :: Monad m => Name -> Linkage -> UnnamedAddr -> LLVMT m Expression -> Alignment -> LLVMT m Expression
defineVariable name linkage addr definition align =
  define =<< lift (variableDefinition name linkage addr definition align)

single :: Applicative f => String -> [a] -> f a
single _ [def] = pure def
single description _ = error $ description ++ " cannot contain other globals"

alias :: Monad m => Name -> Linkage -> UnnamedAddr -> Expression -> Type -> LLVMT m Expression
alias name linkage addr value ty = define [Alias name linkage addr value ty]

attributeGroup :: Monad m => Int -> [String] -> LLVMT m ()
attributeGroup n attrs = globals <>= [AttributeGroup n attrs]

define :: Monad m => [Global] -> LLVMT m Expression
define defs = globalRef (last defs) <$ (globals <>= defs)

placeholder :: Name -> Type -> Expression
placeholder = Placeholder

-- * Statements

call :: Monad m => CallingConvention -> Expression -> [Expression] -> LLVMT m Expression
call = ((assignNew .) .) . Call

call_ :: Monad m => CallingConvention -> Expression -> [Expression] -> LLVMT m ()
call_ = ((state .) .) . VoidCall

store :: Monad m => Expression -> Expression -> LLVMT m ()
store = (state .) . Store

comment :: Monad m => String -> LLVMT m ()
comment = state . Comment

load :: Monad m => Expression -> LLVMT m Expression
load = assignNew . Load

getelementptr :: Monad m => Expression -> [Expression] -> LLVMT m Expression
getelementptr = (assignNew .) . GEP

getelementptr' :: Expression -> [Expression] -> Expression
getelementptr' = ConstGEP

bitcast, ptrtoint, inttoptr, trunc, zext :: Monad m => Expression -> Type -> LLVMT m Expression
bitcast = convert Bitcast
ptrtoint = convert PtrToInt
inttoptr = convert IntToPtr
trunc = convert Trunc
zext = convert Zext

-- TODO: Currently specific to double-precision floats
fptoptr, ptrtofp :: Monad m => Expression -> Type -> LLVMT m Expression
fptoptr a ty = flip inttoptr ty =<< bitcast a (I 64)
ptrtofp p ty = flip bitcast ty =<< ptrtoint p (I 64)

convert :: Monad m => ConversionOp -> Expression -> Type -> LLVMT m Expression
convert _ a ty | typeOf a == ty = pure a
convert op a ty = assignNew $ Convert op a ty

bitcast', ptrtoint', inttoptr', trunc', zext' :: Expression -> Type -> Expression
bitcast' = convert' Bitcast
ptrtoint' = convert' PtrToInt
inttoptr' = convert' IntToPtr
trunc' = convert' Trunc
zext' = convert' Zext

convert' :: ConversionOp -> Expression -> Type -> Expression
convert' _ a ty | typeOf a == ty = a
convert' IntToPtr (ConstConvert Bitcast a _) ty = ConstConvert IntToPtr a ty
-- TODO: fold other cases?
convert' op a ty = ConstConvert op a ty

alloca :: Monad m => Type -> Expression -> LLVMT m Expression
alloca = (assignNew .) . Alloca

andOp, orOp, xorOp, addOp, subOp, mulOp :: Monad m => Expression -> Expression -> LLVMT m Expression
andOp = binaryOp And
orOp = binaryOp Or
xorOp = binaryOp Xor
addOp = binaryOp Add
subOp = binaryOp Sub
mulOp = binaryOp Mul

inc, dec :: Monad m => Expression -> LLVMT m Expression
inc a = addOp a $ integer (typeOf a) 1
dec a = subOp a $ integer (typeOf a) 1

icmp :: Monad m => Comparison -> Expression -> Expression -> LLVMT m Expression
icmp = binaryOp . ICmp

binaryOp :: Monad m => BinaryOp -> Expression -> Expression -> LLVMT m Expression
binaryOp = ((assignNew .) .) . BinaryOp

andOp', orOp', xorOp', addOp', subOp', mulOp' :: Expression -> Expression -> Expression
andOp' = binaryOp' And
orOp' = binaryOp' Or
xorOp' = binaryOp' Xor
addOp' = binaryOp' Add
subOp' = binaryOp' Sub
mulOp' = binaryOp' Mul

inc', dec' :: Expression -> Expression
inc' a = addOp' a $ integer (typeOf a) 1
dec' a = subOp' a $ integer (typeOf a) 1

icmp' :: Comparison -> Expression -> Expression -> Expression
icmp' = binaryOp' . ICmp

binaryOp' :: BinaryOp -> Expression -> Expression -> Expression
binaryOp' = ConstBinaryOp

phi :: Monad m => [(Expression, Name)] -> LLVMT m Expression
phi (pred:preds) = assignNew $ Phi pred preds
phi [] = error "Phi node must have at least one predecessor"

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

br_ :: Monad m => Expression -> Name -> LLVMT m ()
br_ cond target@(Name n) = let def = Name ("not" <> n) in br cond target def >> label def

switch :: Monad m => Expression -> Name -> [(Expression, Name)] -> LLVMT m ()
switch = ((terminator .) .) . Switch

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

i1 :: Bool -> Expression
i1 a = integer (I 1) $ if a then 1 else 0

i4, i8, i32 :: Int -> Expression
i4 = integer (I 4)
i8 = integer (I 8)
i32 = integer (I 32)

i64 :: Integral a => a -> Expression
i64 = integer (I 64)

f64 :: Double -> Expression
f64 = float F64

integer :: Integral a => Type -> a -> Expression
integer ty value = let (name, ty', value') = go ty value in Literal $ IntegerLiteral name ty' value' where
  go (I bits) value = (Nothing, bits, fromIntegral value)
  go (Custom name ty) value = go ty value & _1 .~ Just name
  go ty _ = error $ "not a valid type for an integer literal: " ++ show ty

float :: Real a => Type -> a -> Expression
float ty value = let (name, value') = go ty value in Literal $ FloatLiteral name value' where
  go F64 value = (Nothing, fromRational $ toRational value)
  go (Custom name ty) value = go ty value & _1 .~ Just name
  go ty _ = error $ "not a valid type for a floating-point literal: " ++ show ty

string :: String -> Expression
string = Literal . StringLiteral

undef :: Type -> Expression
undef = Undefined

-- * Utilities

newLocal :: Monad m => LLVMT m Name
newLocal = do lastVar += 1; uses lastVar localName

sizeOf :: Type -> Type -> Expression
sizeOf tySize ty = ptrtoint' (getelementptr' (zeroinitializer $ Ptr ty) [i64 1]) tySize
