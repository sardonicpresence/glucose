module Glucose.TypeChecker where

import Control.Monad
import Control.Monad.State
import Data.Map as Map
import Data.Set as Set

import Glucose.AST as AST
import Glucose.Error
import Glucose.Identifier
import Glucose.IR as IR
import Glucose.Namespace

data TypeChecker = TypeChecker { inlined :: Map Identifier IR.Expression, inlining :: Set Identifier }

mkTypeChecker :: TypeChecker
mkTypeChecker = TypeChecker Map.empty Set.empty

push :: Identifier -> TypeChecker -> TypeChecker
push name tc@TypeChecker{inlining} = tc { inlining = Set.insert name inlining }

define :: Identifier -> IR.Expression -> TypeChecker -> TypeChecker
define name value tc@TypeChecker{inlined,inlining} = tc { inlined = Map.insert name value inlined, inlining = Set.delete name inlining }

type TypeCheck a = StateT TypeChecker Error a

typeCheck :: AST.Module -> Error IR.Module
typeCheck (AST.Module defs) = do
  ns <- foldM (flip declare) emptyNamespace defs
  defs' <- fst <$> runStateT (traverse (typeCheckDefinition ns) defs) mkTypeChecker
  pure $ IR.Module defs'

typeCheckDefinition :: Namespace -> AST.Definition -> TypeCheck IR.Definition
typeCheckDefinition ns (AST.Definition name value _) = IR.Definition name <$> do
  recursive <- gets $ elem name . inlining
  when recursive . throwError $ "recursive definition: the value of " ++ show name ++ " depends on itself"
  modify $ push name
  inlined <- fullyInline ns value
  modify $ define name inlined
  pure inlined

fullyInline :: Namespace -> AST.Expression -> TypeCheck IR.Expression
fullyInline _ (AST.Literal (AST.IntegerLiteral a)) = pure $ IR.Literal (IR.IntegerLiteral a)
fullyInline _ (AST.Literal (AST.FloatLiteral a)) = pure $ IR.Literal (IR.FloatLiteral a)
fullyInline ns (AST.Variable identifier) = do
  value <- gets $ Map.lookup identifier . inlined
  maybe (typeCheckIdentifier ns identifier) pure value

typeCheckIdentifier :: Namespace -> Identifier -> TypeCheck IR.Expression
typeCheckIdentifier ns identifier = case definitionOf identifier ns of
  Nothing -> throwError $ "unrecognised variable " ++ show identifier
  Just def -> valueOf <$> typeCheckDefinition ns def

valueOf :: IR.Definition -> IR.Expression
valueOf (IR.Definition _ value) = value
