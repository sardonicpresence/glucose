module Glucose.Namespace where

import Control.Applicative
import Data.Map.Strict as Map
import qualified Glucose.IR as IR
import Glucose.Error
import Glucose.Identifier
import Glucose.Lexer.Location
import Glucose.Parser.Source

data Variable = Arg (FromSource IR.Arg) | Definition (FromSource IR.Definition)

instance Bound Variable where
  identifier (Arg (FromSource _ arg)) = identifier arg
  identifier (Definition (FromSource _ def)) = identifier def

locationOf :: Variable -> Location
locationOf (Arg a) = startLocation a
locationOf (Definition a) = startLocation a

newtype Scope = Scope (Map Identifier Variable)

newtype Namespace = Namespace [Scope]

data ScopeLevel = TopLevel | CurrentScope | ParentScope

emptyNamespace :: Namespace
emptyNamespace = Namespace [Scope Map.empty]

pushScope :: Namespace -> Namespace
pushScope (Namespace scopes) = Namespace $ Scope Map.empty : scopes

popScope :: Namespace -> Namespace
popScope (Namespace scopes) = Namespace $ tail scopes

declare :: MonadThrow CompileError m => Variable -> Namespace -> m Namespace
declare var ns = case lookupVariable (identifier var) ns of
  Nothing -> pure $ declare_ var ns
  Just (CurrentScope, prev) -> duplicateDefinition (locationOf var) (identifier var) (locationOf prev)
  Just _ -> pure $ declare_ var ns -- TODO: warn about name shadowing

declare_ :: Variable -> Namespace -> Namespace
declare_ var (Namespace []) = Namespace [Scope $ Map.insert (identifier var) var Map.empty]
declare_ var (Namespace (Scope s : ss)) = Namespace $ (Scope $ Map.insert (identifier var) var s) : ss

lookupVariable :: Identifier -> Namespace -> Maybe (ScopeLevel, Variable)
lookupVariable _ (Namespace []) = Nothing
lookupVariable n (Namespace (Scope s:ss)) = (CurrentScope, ) <$> Map.lookup n s <|> go ss where
  go [] = Nothing
  go [Scope s] = (TopLevel, ) <$> Map.lookup n s
  go (Scope s:ss) = (ParentScope, ) <$> Map.lookup n s <|> go ss

lookupDefinition :: Identifier -> Namespace -> Maybe (FromSource IR.Definition)
lookupDefinition n (Namespace ss) = go ss where
  go [] = Nothing
  go (Scope s : ss) = (Map.lookup n s >>= \case Arg _ -> Nothing; Definition def -> Just def) <|> go ss
