module Glucose.Namespace
(
  Scope, Namespace, emptyNamespace, pushScope, popScope, declare, lookupVariable, lookupDefinition
)
where

import Control.Applicative
import Data.Map.Strict as Map
import qualified Glucose.IR as IR
import Glucose.Error
import Glucose.Identifier
import Glucose.Parser.Source

type Definition = IR.Definition IR.Checked

newtype Scope = Scope (Map Identifier (FromSource Definition))

newtype Namespace = Namespace [Scope]

data ScopeLevel = TopLevel | CurrentScope | ParentScope

emptyNamespace :: Namespace
emptyNamespace = Namespace [Scope Map.empty]

pushScope :: Namespace -> Namespace
pushScope (Namespace scopes) = Namespace $ Scope Map.empty : scopes

popScope :: Namespace -> Namespace
popScope (Namespace scopes) = Namespace $ tail scopes

declare :: MonadThrow CompileError m => FromSource Definition -> Namespace -> m Namespace
declare def ns = case lookupVariable (identifier def) ns of
  Nothing -> pure $ declare_ def ns
  Just (CurrentScope, prev) -> duplicateDefinition (startLocation def) (identifier def) (startLocation prev)
  Just _ -> pure $ declare_ def ns -- TODO: warn about name shadowing

declare_ :: FromSource Definition -> Namespace -> Namespace
declare_ def (Namespace []) = Namespace [Scope $ Map.insert (identifier def) def Map.empty]
declare_ def (Namespace (Scope s : ss)) = Namespace $ (Scope $ Map.insert (identifier def) def s) : ss

lookupVariable :: Identifier -> Namespace -> Maybe (ScopeLevel, FromSource Definition)
lookupVariable _ (Namespace []) = Nothing
lookupVariable n (Namespace (Scope s:ss)) = (CurrentScope, ) <$> Map.lookup n s <|> go ss where
  go [] = Nothing
  go [Scope s] = (TopLevel, ) <$> Map.lookup n s
  go (Scope s:ss) = (ParentScope, ) <$> Map.lookup n s <|> go ss

lookupDefinition :: Identifier -> Namespace -> Maybe (FromSource Definition)
lookupDefinition n (Namespace ss) = go ss where
  go [] = Nothing
  go (Scope s : ss) = Map.lookup n s <|> go ss
