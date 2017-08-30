module Glucose.IR.Checked ( module Glucose.IR, module Glucose.IR.Checked ) where

import Control.Comonad.Utils
import Control.Lens
import Data.Monoid
import qualified Data.Set as Set
import Glucose.IR hiding (Module(), Definition(), Expression(), Arg(), Type())
import qualified Glucose.IR as IR

type Module = IR.Module IR.Checked
type Definition = IR.Definition IR.Checked
type Expression = IR.Expression IR.Checked
type Arg = IR.Arg IR.Checked
type Type = IR.Type IR.Checked

flatten :: Comonad f => Expression f -> Expression f -> (Expression f, [Expression f])
flatten f x = go f [x] where
  go f as = case f of
    Apply g a _ -> go (extract g) (extract a : as)
    _ -> (f, as)

-- * Apply

-- | Chain of function applications with an optional trailing partial application
data Application f = Application Type (Expression f) [[Expression f]] (Maybe (Partial f)) deriving (Show)
data Partial f = Partial Type [Expression f] deriving (Show)

flattenApply :: Comonad f => Expression f -> Expression f -> Application f
flattenApply f a = go f [a] where
  go f args = case f of
    Apply (extract -> g) (extract -> x) _ -> go g (x:args) -- TODO: use type?
    Reference _ _ rep ty -> uncurry (Application rep f) (apply ty [] args)
    Lambda _ _ -> undefined
    Literal _ -> error "Cannot supply arguments to a literal!" -- TODO: improve & move
  -- apply :: Type -> [Expression f] -> [Expression f] -> ([[Expression f]], Maybe (Partial f))
  apply (CheckedType (Function (Arity 1) _ b)) applied (a:as) = apply b [] as & _1 %~ (reverse (a:applied) :)
  apply (CheckedType (Function (Arity _) _ b)) applied (a:as) = apply b (a:applied) as
  apply _ [] [] = ([], Nothing)
  apply b applied [] = ([], Just . Partial b $ reverse applied)
  apply b _ _ = error $ "Cannot supply arguments to non-function " ++ show b -- TODO: improve & move

captures :: Comonad f => Expression f -> Set.Set Arg
captures (Reference Local name _ ty) = Set.singleton $ Arg name ty
captures (Apply expr arg _) = captures (extract expr) <> captures (extract arg)
captures (Lambda args value) = captures (extract value) Set.\\ Set.fromList (map extract args)
captures _ = mempty

effectiveArity :: Type -> Int
effectiveArity (CheckedType (Function (Arity n) _ _)) = n
effectiveArity (CheckedType (Function _ _ b)) = 1 + effectiveArity b
effectiveArity _ = 0
