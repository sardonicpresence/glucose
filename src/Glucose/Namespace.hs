module Glucose.Namespace
(
  Variable(..), Namespace, emptyNamespace, pushScope, popScope,
  declare, declareArg, declareDefinition,
  lookupDefinition, lookupVariable
)
where

import Control.Applicative
import Control.Comonad
import Control.Monad.Except
import Data.Map.Strict as Map
import qualified Glucose.IR as IR
import Glucose.Identifier

type Arg = IR.Arg IR.Checked
type Definition = IR.Definition IR.Checked

data Variable f = Arg (f Arg) | Definition (f (Definition f))

instance Comonad f => Bound (Variable f) where
  identifier (Arg arg) = identifier $ extract arg
  identifier (Definition def) = identifier $ extract def

newtype Scope f = Scope (Map Identifier (Variable f))

newtype Namespace f = Namespace [Scope f]

data ScopeLevel = TopLevel | CurrentScope | ParentScope

emptyNamespace :: Namespace f
emptyNamespace = Namespace [Scope Map.empty]

pushScope :: Namespace f -> Namespace f
pushScope (Namespace scopes) = Namespace $ Scope Map.empty : scopes

popScope :: Namespace f -> Namespace f
popScope (Namespace scopes) = Namespace $ tail scopes

declare :: Comonad f => Variable f -> Namespace f -> Either (Variable f) (Namespace f)
declare var ns = case lookupVariable (identifier var) ns of
  Nothing -> pure $ declare_ var ns
  Just (CurrentScope, prev) -> throwError prev -- duplicate definition
  Just _ -> pure $ declare_ var ns -- TODO: warn about name shadowing

declareArg :: Comonad f => f Arg -> Namespace f -> Either (Variable f) (Namespace f)
declareArg = declare . Arg

declareDefinition :: Comonad f => f (Definition f) -> Namespace f -> Either (Variable f) (Namespace f)
declareDefinition = declare . Definition

declare_ :: Comonad f => Variable f -> Namespace f -> Namespace f
declare_ var (Namespace []) = Namespace [Scope $ Map.insert (identifier var) var Map.empty]
declare_ var (Namespace (Scope s : ss)) = Namespace $ (Scope $ Map.insert (identifier var) var s) : ss

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
