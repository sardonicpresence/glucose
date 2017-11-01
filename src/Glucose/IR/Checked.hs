module Glucose.IR.Checked
(
  module Glucose.IR,
  Module, Definition, Expression, Arg, Type,
  Call, Partial(..), Application(..),
  flatten, groupApplication, captures, effectiveArity
)
where

import Control.Applicative
import Control.Comonad.Utils
import Data.List (intercalate)
import Data.Semigroup (Semigroup(..))
import qualified Data.Set as Set
import Glucose.Format
import Glucose.IR.Format (withType)
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

type Call a = [(Type, a)]
data Partial a = Partial Int (Call a)
data Application a = Application Type [Call a] (Maybe (Partial a))

showCall args = "(" <> intercalate "," (map (\(ty, a) -> show $ a `withType` ty) args) <> ")"

instance Formattable Format a => Show (Application a) where
  show (Application _ calls Nothing) = "f" <> concatMap showCall calls
  show (Application ty calls (Just (Partial n call))) = let lambdaArgs = map (("$"<>) . show) [1..n] in
    "\\" <> unwords lambdaArgs <> " -> " <> show (Application ty calls Nothing) <> "(" <>
    intercalate ", " (map (\(ty, a) -> show $ a `withType` ty) call ++ lambdaArgs) <> ")"

instance Semigroup (Application a) where
  (Application _ as b) <> (Application ty cs d) = Application ty (as <> cs) (b <|> d)
  -- TODO: Use of <|> is a bit dodgy as we should never have 2 partial applications

fullApplication :: Type -> Call a -> Application a
fullApplication result as = Application result [as] Nothing

partialApplication :: Type -> Int -> Call a -> Application a
partialApplication result arity = Application result [] . Just . Partial arity

{- | Groups arguments into one or more calls given a function type to apply them to.
 - Each call is returned as a list of arguments paired with their expected types.
 -}
groupApplication :: Type -> [a] -> Application a
groupApplication ty = go ty [] where
  go ty [] [] = Application ty [] Nothing
  go ty@(CheckedType (Function (Arity arity) _ _)) as [] | arity > 0 = partialApplication ty arity as
  go (CheckedType (Function arity a b)) as (r:rs) =
    let as' = (a, r) : as in
    case arity of
      Arity 1 -> fullApplication b (reverse as') <> go b [] rs
      _ -> go b as' rs
  go ty as [] = fullApplication ty $ reverse as
  go ty _ bs = error $ "Cannot apply " <> show (length bs) <> " arguments to expression of type " <> show ty

captures :: Comonad f => Expression f -> Set.Set Arg
captures (Reference (Local name) ty) = Set.singleton $ Arg name ty
captures (Apply expr arg _) = captures (extract expr) <> captures (extract arg)
captures (Lambda arg value) = captures (extract value) Set.\\ Set.fromList [extract arg]
captures _ = mempty

effectiveArity :: Type -> Int
effectiveArity (CheckedType (Function (Arity n) _ _)) = n
effectiveArity (CheckedType (Function _ _ b)) = 1 + effectiveArity b
effectiveArity _ = 0
