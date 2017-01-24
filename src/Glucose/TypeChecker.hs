module Glucose.TypeChecker where

import Control.Comonad
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Map.Strict as Map
import Data.Set as Set

import qualified Glucose.AST as AST
import Glucose.Error
import Glucose.Identifier
import qualified Glucose.IR as IR
import Glucose.Namespace
import Glucose.Parser.Source
import Glucose.VarGen

data TypeChecker = TypeChecker
  { namespace :: Namespace
  , checking :: Set Identifier
  , nextVar :: VarGen
  , unchecked :: Map Identifier (FromSource AST.Definition) }

_namespace :: Lens TypeChecker TypeChecker Namespace Namespace
_namespace = lens namespace (\a b -> a { namespace = b })

_checking :: Lens TypeChecker TypeChecker (Set Identifier) (Set Identifier)
_checking = lens checking (\a b -> a { checking = b })

type TypeCheck m a = StateT TypeChecker m a

_nextVar :: Lens' TypeChecker VarGen
_nextVar = lens nextVar (\a b -> a { nextVar = b})

mkTypeChecker :: Error m => [FromSource AST.Definition] -> m TypeChecker
mkTypeChecker defs = TypeChecker emptyNamespace Set.empty mkVarGen <$> toMap defs where
  toMap = foldM insertInto Map.empty
  insertInto m def = insertOrError m (identifier def) def $
    duplicateDefinition (startLocation def) (identifier def) . startLocation
  insertOrError m k v formatError = case Map.insertLookupWithKey noReplace k v m of
    (Nothing, m') -> pure m'
    (Just existing, _) -> formatError existing
  noReplace _ _ a = a

push :: Error m => Identifier -> TypeCheck m ()
push name = _checking %= Set.insert name

pushArgs :: Error m => [FromSource IR.Arg] -> TypeCheck m ()
pushArgs args = do
  ns <- gets namespace
  ns' <- foldM (flip (declare . Arg)) (pushScope ns) args
  modify $ \tc -> tc { namespace = ns' }

popArgs :: Error m => TypeCheck m ()
popArgs = _namespace %= popScope

define :: Error m => Identifier -> FromSource IR.Definition -> TypeCheck m (FromSource IR.Definition)
define name value = do
  tc@TypeChecker{namespace,unchecked,checking} <- get
  namespace' <- declare (Definition value) namespace
  put tc { namespace = namespace', unchecked = Map.delete name unchecked, checking = Set.delete name checking }
  pure value

typeCheck :: Error m => AST.Module -> m IR.Module
typeCheck (AST.Module defs) = (IR.Module <$>) . evalStateT (traverse go defs) =<< mkTypeChecker defs where
  go def = do
    existing <- gets $ lookupDefinition (identifier def) . namespace
    maybe (typeCheckDefinition def) pure existing

typeCheckDefinition :: Error m => FromSource AST.Definition -> TypeCheck m (FromSource IR.Definition)
typeCheckDefinition def = define (identifier def) =<< traverse go def where
  go :: Error m => AST.Definition -> TypeCheck m IR.Definition
  go (AST.Definition name value) = do
    recursive <- gets $ elem (extract name) . checking
    when recursive $ recursiveDefinition (startLocation name) (identifier name)
    push $ extract name
    _nextVar .= mkVarGen
    IR.Definition name <$> traverse typeCheckExpression (duplicate value)

typeCheckExpression :: Error m => FromSource AST.Expression -> TypeCheck m IR.Expression
typeCheckExpression expr = case extract expr of
  AST.Value value -> typeCheckValue $ value <$ expr
  AST.Apply expr arg -> do
    expr' <- traverse typeCheckExpression $ duplicate expr
    arg' <- traverse typeCheckValue $ duplicate arg
    (f, g) <- unify (IR.typeOf <$> expr') (functionOf . IR.typeOf <$> arg')
    pure $ IR.Apply (f <$> expr') (g <$> arg')

typeCheckValue :: Error m => FromSource AST.Value -> TypeCheck m IR.Expression
typeCheckValue expr = case extract expr of
  AST.Literal (AST.IntegerLiteral a) -> pure $ IR.Literal (IR.IntegerLiteral a)
  AST.Literal (AST.FloatLiteral a) -> pure $ IR.Literal (IR.FloatLiteral a)
  AST.Variable identifier -> do
    value <- gets $ lookupVariable identifier . namespace
    maybe (typeCheckIdentifier $ identifier <$ expr) fromVariable $ snd <$> value
  AST.Lambda args def -> do
    typeCheckedArgs <- traverse (traverse typeCheckArg) args
    pushArgs typeCheckedArgs
    retType <- IR.Bound <$> newVar
    def' <- traverse typeCheckExpression (duplicate def)
    def'' <- (<$> def') . snd <$> unify (IR.typeOf <$> def') (retType <$ def)
    def''' <- traverse (IR.bindTypes newVar) def''
    pure $ IR.Lambda typeCheckedArgs def'''

typeCheckArg :: Error m => Identifier -> TypeCheck m IR.Arg
typeCheckArg name = IR.Arg name . IR.Bound <$> newVar

typeCheckIdentifier :: Error m => FromSource Identifier -> TypeCheck m IR.Expression
typeCheckIdentifier variable = gets unchecked >>= \ns -> case Map.lookup (identifier variable) ns of
  Nothing -> unrecognisedVariable (startLocation variable) (identifier variable)
  Just def -> referenceTo =<< extract <$> typeCheckDefinition def

unify :: Error m => FromSource IR.Type -> FromSource IR.Type -> m (IR.Expression -> IR.Expression, IR.Expression -> IR.Expression)
unify ty1 ty2 = go (extract ty1) (extract ty2) where
  go (IR.Function _ f a) (IR.Function _ g b) = do
    (h1, h2) <- unify (f <$ ty1) (g <$ ty2)
    (i1, i2) <- unify (a <$ ty1) (b <$ ty2)
    pure (i1 . h1, i2 . h2)
  go ty (IR.Free name) = pure (id, IR.bindType name ty)
  go (IR.Free name) ty = pure (IR.bindType name ty, id)
  go ty (IR.Bound name) = pure (id, IR.rebindType name ty)
  go (IR.Bound name) ty = pure (IR.rebindType name ty, id)
  go a b = when (a /= b) (typeMismatch (startLocation ty2) a b) *> pure (id, id)

newVar :: Monad m => TypeCheck m Identifier
newVar = do
  (var, nextVar') <- genVar <$> gets (^. _nextVar)
  _nextVar .= nextVar'
  pure var

functionOf :: IR.Type -> IR.Type
functionOf a = IR.Function IR.UnknownArity a $ IR.Free $ Identifier "_"

fromVariable :: Monad m => Variable -> TypeCheck m IR.Expression
fromVariable (Definition def) = referenceTo $ extract def
fromVariable (Arg arg) = pure $ referenceArg $ extract arg

referenceArg :: IR.Arg -> IR.Expression
referenceArg (IR.Arg name ty) = IR.Reference IR.Local name ty

referenceTo :: Monad m => IR.Definition -> TypeCheck m IR.Expression
referenceTo def = IR.freeTypes newVar $ IR.Reference IR.Global (identifier def) (IR.typeOf def)

valueOf :: IR.Definition -> IR.Expression
valueOf (IR.Definition _ value) = extract value
