module Glucose.Namespace
(
  Variable(..), Namespace, emptyNamespace, pushScope, popScope, declaredArgs,
  declare, declareArg, declareDefinition,
  lookupDefinition, lookupVariable
)
where

import Control.Applicative
import Control.Comonad
import Control.Lens
import Control.Monad.Except
import Data.Map.Strict as Map
import qualified Glucose.IR as IR
import Glucose.Identifier

type Arg = IR.Arg IR.Checking
type Definition = IR.Definition IR.Checked

data Variable f = Arg (f Arg) | Definition (f (Definition f))

instance Comonad f => Bound f (Variable f) where
  identifier (Arg arg) = identifier arg
  identifier (Definition def) = identifier $ extract def

newtype Scope f = Scope (Map Identifier (Variable f))

newtype Namespace f = Namespace [Scope f]

declaredArgs :: Traversal' (Namespace f) (f Arg)
declaredArgs f (Namespace scopes) = Namespace <$> traverse (\(Scope s) -> Scope <$> traverse (\case Arg a -> Arg <$> f a; a -> pure a) s) scopes

data ScopeLevel = TopLevel | CurrentScope | ParentScope

emptyNamespace :: Namespace f
emptyNamespace = Namespace [Scope Map.empty]

pushScope :: Namespace f -> Namespace f
pushScope (Namespace scopes) = Namespace $ Scope Map.empty : scopes

popScope :: Namespace f -> ([Variable f], Namespace f)
popScope (Namespace (Scope scope : scopes)) = (Map.elems scope, Namespace scopes)

declare :: Comonad f => Variable f -> Namespace f -> Either (Variable f) (Namespace f)
declare var ns = case lookupVariable (identify var) ns of
  Nothing -> pure $ declareUnsafe var ns
  Just (CurrentScope, prev) -> throwError prev -- duplicate definition
  Just _ -> pure $ declareUnsafe var ns -- TODO: warn about name shadowing
  where
    declareUnsafe var (Namespace []) = Namespace [Scope $ Map.insert (identify var) var Map.empty]
    declareUnsafe var (Namespace (Scope s : ss)) = Namespace $ (Scope $ Map.insert (identify var) var s) : ss

declareArg :: Comonad f => f Arg -> Namespace f -> Either (Variable f) (Namespace f)
declareArg = declare . Arg

declareDefinition :: Comonad f => f (Definition f) -> Namespace f -> Either (Variable f) (Namespace f)
declareDefinition = declare . Definition

lookupVariable :: Identifier -> Namespace f -> Maybe (ScopeLevel, Variable f)
lookupVariable _ (Namespace []) = Nothing
lookupVariable n (Namespace (Scope s:ss)) = (CurrentScope, ) <$> Map.lookup n s <|> go ss where
  go [] = Nothing
  go [Scope s] = (TopLevel, ) <$> Map.lookup n s
  go (Scope s:ss) = (ParentScope, ) <$> Map.lookup n s <|> go ss

lookupDefinition :: Identifier -> Namespace f -> Maybe (f (Definition f))
lookupDefinition n (Namespace ss) = go ss where
  go [] = Nothing
  go (Scope s : ss) = (Map.lookup n s >>= \case Definition def -> Just def; _ -> Nothing) <|> go ss
