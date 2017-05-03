module Glucose.IR.Checked
(
  module Glucose.IR,
  module Glucose.IR.Checked
)
where

import Control.Comonad
import Control.Lens
import Data.List
import Data.Monoid
import qualified Data.Set as Set
import Glucose.Identifier
import Glucose.IR hiding (Module(), Definition(), Expression(), Arg(), Type())
import qualified Glucose.IR as IR

type Module = IR.Module IR.Checked
type Definition = IR.Definition IR.Checked
type Expression = IR.Expression IR.Checked
type Arg = IR.Arg IR.Checked
type Type = IR.Type IR.Checked

-- * Apply

-- | Chain of function applications with an optional trailing partial application
data Application = Application Type Expression [[Expression]] (Maybe Partial) deriving (Show)
data Partial = Partial Type [Expression] deriving (Show)

flattenApply :: Expression -> Expression -> Application
flattenApply f a = go f [a] where
  go f args = case f of
    Apply (extract -> g) (extract -> x) -> go g (x:args)
    Reference _ _ rep ty -> uncurry (Application rep f) (apply ty [] args)
    Lambda _ _ -> undefined
    Constructor _ _ -> error "Cannot supply arguments to a constructor"
    Literal _ -> error "Cannot supply arguments to a literal!" -- TODO: improve & move
  -- apply :: Type -> [Expression] -> ([[Expression]], Maybe [Expression])
  apply (Function (Arity 1) _ b) applied (a:as) = apply b [] as & _1 %~ (reverse (a:applied) :)
  apply (Function (Arity _) _ b) applied (a:as) = apply b (a:applied) as
  apply _ [] [] = ([], Nothing)
  apply b applied [] = ([], Just . Partial b $ reverse applied)
  apply _ _ _ = error "Cannot supply arguments to a non-function!" -- TODO: improve & move

-- * Type lensing

free :: Prism' Type Identifier
free = prism' Free $ \case Free a -> Just a; _ -> Nothing

bound :: Prism' Type Identifier
bound = prism' Bound $ \case Bound a -> Just a; _ -> Nothing

types :: Traversal' Expression Type
types f a@Literal{} = f (typeOf a) $> a
types f (Reference kind name rep ty) = Reference kind name rep <$> prims f ty
types f (Constructor tyName idx) = injectType <$> prims f (ADT $ extract tyName) where
  injectType ty' = let tyName' = case ty' of ADT n' -> tyName $> n'; _ -> tyName in Constructor tyName' idx
types f (Lambda args expr) = Lambda <$> traverse (traverse $ argType f) args <*> traverse (types f) expr where
  argType f (Arg name ty) = Arg name <$> prims f ty
types f (Apply fun arg) = Apply <$> traverse (types f) fun <*> traverse (types f) arg

prims :: Traversal' Type Type
prims f (Function rep a b) = Function rep <$> prims f a <*> prims f b
prims f a = f a

remapTypes :: Monad m => m Identifier -> Prism' Type Identifier -> Prism' Type Identifier -> Expression -> m Expression
remapTypes replacement from to expr = do
  subs <- traverse (\a -> (a ^. re from,) <$> replacement) . nub $ expr ^.. types . from
  expr & types %%~ \a -> pure . maybe a (^. re to) $ lookup a subs

freeTypes :: Monad m => m Identifier -> Expression -> m Expression
freeTypes newVar = remapTypes newVar bound free

bindTypes :: Monad m => m Identifier -> Expression -> m Expression
bindTypes newVar = remapTypes newVar free bound

bindType :: Identifier -> Type -> Expression -> Expression
bindType name = set (types . filtered (== Free name)) . boxed

rebindType :: Identifier -> Type -> Expression -> Expression
rebindType name = set (types . filtered (== Bound name)) . boxed

boxed :: Type -> Type
boxed Integer = Boxed Integer
boxed Float = Boxed Float
boxed a = a

captures :: Expression -> Set.Set Arg
captures (Reference Local name _ ty) = Set.singleton $ Arg name ty
captures (Apply expr arg) = captures (extract expr) <> captures (extract arg)
captures (Lambda args value) = captures (extract value) Set.\\ Set.fromList (map extract args)
captures _ = mempty
