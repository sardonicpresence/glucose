module Glucose.TypeChecker where

import Control.Comonad
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Map.Strict as Map
import Data.Map.Utils
import Data.Set as Set

import Glucose.Desugar
import Glucose.Error
import Glucose.Identifier
import Glucose.IR
import Glucose.IR.Checked (bindTypes, bindType, rebindType, freeTypes)
import Glucose.Namespace hiding (Arg, Definition)
import qualified Glucose.Namespace as NS
import Glucose.Source
import Glucose.VarGen

data TypeChecker = TypeChecker
  { namespace :: Namespace
  , constructors :: Map (Identifier, Int) Location
  , checking :: Set Identifier
  , nextVar :: VarGen
  , unchecked :: Map Identifier (Desugared Definition) }

_namespace :: Lens' TypeChecker Namespace
_namespace = lens namespace (\a b -> a { namespace = b })

_constructors :: Lens' TypeChecker (Map (Identifier, Int) Location)
_constructors = lens constructors (\a b -> a { constructors = b })

_checking :: Lens' TypeChecker (Set Identifier)
_checking = lens checking (\a b -> a { checking = b })

_nextVar :: Lens' TypeChecker VarGen
_nextVar = lens nextVar (\a b -> a { nextVar = b})

mkTypeChecker :: Error m => [Desugared Definition] -> m TypeChecker
mkTypeChecker = (TypeChecker emptyNamespace Map.empty Set.empty mkVarGen <$>) . bindings reportDuplicate where
    reportDuplicate def = duplicateDefinition (startLocation def) (identifier def) . startLocation

push :: Error m => Identifier -> TypeCheck m ()
push name = _checking %= Set.insert name

pushArgs :: Error m => [FromSource (Arg Checked)] -> TypeCheck m ()
pushArgs args = do
  ns <- gets namespace
  ns' <- foldM (flip declareArg) (pushScope ns) args
  modify $ \tc -> tc { namespace = ns' }

popArgs :: Error m => TypeCheck m ()
popArgs = _namespace %= popScope

define :: Error m => Identifier -> FromSource (Definition Checked) -> TypeCheck m (FromSource (Definition Checked))
define name value = do
  tc@TypeChecker{namespace,unchecked,checking} <- get
  namespace' <- declareDefinition value namespace
  put tc { namespace = namespace', unchecked = Map.delete name unchecked, checking = Set.delete name checking }
  pure value

type TypeCheck m a = StateT TypeChecker m a

typeCheck :: Error m => Module Unchecked -> m (Module Checked)
typeCheck (Module defs) = Module <$> (evalStateT (traverse go defs) =<< mkTypeChecker defs) where
  go def = do
    existing <- gets $ lookupDefinition (identifier def) . namespace
    maybe (typeCheckDefinition def) pure existing

typeCheckDefinition :: Error m => FromSource (Definition Unchecked) -> TypeCheck m (FromSource (Definition Checked))
typeCheckDefinition def = define (identifier def) =<< traverse go def where
  go :: Error m => Definition Unchecked -> TypeCheck m (Definition Checked)
  go (Definition name value) = do
    recursive <- gets $ elem (extract name) . checking
    when recursive $ recursiveDefinition (startLocation name) (identifier name)
    push $ extract name
    _nextVar .= mkVarGen
    Definition name <$> traverse typeCheckExpression (duplicate value)

typeCheckExpression :: Error m => FromSource (Expression Unchecked) -> TypeCheck m (Expression Checked)
typeCheckExpression expr = case extract expr of
  Literal literal -> pure $ Literal literal
  Reference _ identifier _ _ -> do
    value <- gets $ lookupVariable identifier . namespace
    maybe (typeCheckIdentifier $ identifier <$ expr) fromVariable $ snd <$> value
  Constructor typeName index -> do
    let key = (extract typeName, index)
    let formatError = duplicateDefinition (startLocation typeName) (extract typeName)
    m <- gets constructors
    m' <- insertOr key (startLocation typeName) formatError m
    _constructors .= m'
    pure $ Constructor typeName index
  Lambda args def -> do
    typeCheckedArgs <- traverse (traverse typeCheckArg) args
    pushArgs typeCheckedArgs
    retType <- Bound <$> newVar
    def' <- traverse typeCheckExpression (duplicate def)
    def'' <- (<$> def') . snd <$> unify (typeOf <$> def') (retType <$ def)
    def''' <- traverse (bindTypes newVar) def''
    pure $ Lambda typeCheckedArgs def'''
  Apply expr arg -> do
    expr' <- traverse typeCheckExpression $ duplicate expr
    arg' <- traverse typeCheckExpression $ duplicate arg
    (f, g) <- unify (typeOf <$> expr') (functionOf . typeOf <$> arg')
    pure $ Apply (f <$> expr') (g <$> arg')

typeCheckIdentifier :: Error m => FromSource Identifier -> TypeCheck m (Expression Checked)
typeCheckIdentifier variable = gets unchecked >>= \ns -> case Map.lookup (identifier variable) ns of
  Nothing -> unrecognisedVariable (startLocation variable) (identifier variable)
  Just def -> referenceTo =<< extract <$> typeCheckDefinition def

typeCheckArg :: Error m => Arg Unchecked -> TypeCheck m (Arg Checked)
typeCheckArg (Arg name _) = Arg name . Bound <$> newVar

unify :: Error m => FromSource (Type Checked) -> FromSource (Type Checked)
                 -> m (Expression Checked -> Expression Checked, Expression Checked -> Expression Checked)
unify ty1 ty2 = go (extract ty1) (extract ty2) where
  go (Function _ f a) (Function _ g b) = do
    (h1, h2) <- unify (f <$ ty1) (g <$ ty2)
    (i1, i2) <- unify (a <$ ty1) (b <$ ty2)
    pure (i1 . h1, i2 . h2)
  go ty (Free name) = pure (id, bindType name ty)
  go (Free name) ty = pure (bindType name ty, id)
  go ty (Bound name) = pure (id, rebindType name ty)
  go (Bound name) ty = pure (rebindType name ty, id)
  go a b = when (a /= b) (typeMismatch (startLocation ty2) a b) *> pure (id, id)

newVar :: Monad m => TypeCheck m Identifier
newVar = do
  (var, nextVar') <- genVar <$> gets (^. _nextVar)
  _nextVar .= nextVar'
  pure var

functionOf :: Type Checked -> Type Checked
functionOf a = Function UnknownArity a $ Free $ Identifier "_"

fromVariable :: Monad m => Variable -> TypeCheck m (Expression Checked)
fromVariable (NS.Definition def) = referenceTo $ extract def
fromVariable (NS.Arg arg) = pure $ referenceArg $ extract arg

referenceArg :: Arg Checked -> Expression Checked
referenceArg (Arg name ty) = Reference Local name ty ty

referenceTo :: Monad m => Definition Checked -> TypeCheck m (Expression Checked)
referenceTo def = freeTypes newVar $ Reference Global (identifier def) (typeOf def) (typeOf def)

valueOf :: Definition Checked -> Expression Checked
valueOf (Definition _ value) = extract value
