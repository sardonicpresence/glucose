module Glucose.TypeChecker where

import Control.Comonad
import Control.Monad.Except
import Control.Lens
import Control.Monad.State
import Data.Map.Strict as Map
import Data.Set as Set

import Glucose.Desugar
import Glucose.Identifier
import Glucose.IR
import Glucose.IR.Checked (bindTypes, bindType, rebindType, freeTypes)
import Glucose.Namespace hiding (Arg, Definition)
import qualified Glucose.Namespace as NS
import Glucose.TypeChecker.TypeCheckError
import Glucose.VarGen

data TypeChecker f = TypeChecker
  { namespace :: Namespace f
  , constructors :: Set (Identifier, Int)
  , checking :: Set Identifier
  , nextVar :: VarGen
  , unchecked :: Map Identifier (Desugared f Definition) }

_namespace :: Lens' (TypeChecker f) (Namespace f)
_namespace = lens namespace (\a b -> a { namespace = b })

_checking :: Lens' (TypeChecker f) (Set Identifier)
_checking = lens checking (\a b -> a { checking = b })

_nextVar :: Lens' (TypeChecker f) VarGen
_nextVar = lens nextVar (\a b -> a { nextVar = b})

mkTypeChecker :: (Comonad f, MonadError (TypeCheckError f) m) => [Desugared f Definition] -> m (TypeChecker f)
mkTypeChecker = (TypeChecker emptyNamespace Set.empty Set.empty mkVarGen <$>) . bindings reportDuplicate (identifier . extract) where
  reportDuplicate def = throwError . DuplicateDefinition (identifier <$> def) . void

push :: MonadError (TypeCheckError f) m => Identifier -> TypeCheck f m ()
push name = _checking %= Set.insert name

pushArgs :: (Comonad f, MonadError (TypeCheckError f) m) => [f (Arg Checked)] -> TypeCheck f m ()
pushArgs args = do
  ns <- gets namespace
  ns' <- foldM (\ns value -> handleDuplicate value $ declareArg value ns) (pushScope ns) args
  modify $ \tc -> tc { namespace = ns' }

popArgs :: MonadError (TypeCheckError f) m => TypeCheck f m ()
popArgs = _namespace %= popScope

define :: (Comonad f, MonadError (TypeCheckError f) m) => Identifier -> f (Definition Checked f) -> TypeCheck f m (f (Definition Checked f))
define name value = do
  tc@TypeChecker{namespace,unchecked,checking} <- get
  namespace' <- handleDuplicate value $ declareDefinition value namespace
  put tc { namespace = namespace', unchecked = Map.delete name unchecked, checking = Set.delete name checking }
  pure value

handleDuplicate :: (Bound a, Functor f, MonadError (TypeCheckError f) m) => f a -> Either (Variable f) (Namespace f) -> m (Namespace f)
handleDuplicate a = either (throwError . DuplicateDefinition (identifier <$> a) . existing) pure where
  existing (NS.Arg a) = void a
  existing (NS.Definition a) = void a

type TypeCheck f m a = StateT (TypeChecker f) m a

typeCheck :: (Comonad f, Traversable f, MonadError (TypeCheckError f) m) => Module Unchecked f -> m (Module Checked f)
typeCheck (Module defs) = Module <$> (evalStateT (traverse go defs) =<< mkTypeChecker defs) where
  go def = do
    existing <- gets $ lookupDefinition (identifier $ extract def) . namespace
    maybe (typeCheckDefinition def) pure existing

typeCheckDefinition :: (Comonad f, Traversable f, MonadError (TypeCheckError f) m) => f (Definition Unchecked f) -> TypeCheck f m (f (Definition Checked f))
typeCheckDefinition def = define (identifier $ extract def) =<< traverse go def where
  go (Definition name value) = do
    recursive <- gets $ elem (extract name) . checking
    when recursive $ throwError $ RecursiveDefinition name
    push $ extract name
    _nextVar .= mkVarGen
    Definition name <$> traverse typeCheckExpression (duplicate value)
  go (Constructor name typeName index) = do
    -- TODO: check for duplicate types (constructors)
    -- tc@TypeChecker{namespace,constructors,unchecked} <- get
    -- let key = (extract typeName, index)
    -- when (Set.member key constructors) $ throwError . DuplicateDefinition (Definition def)
    pure $ Constructor name typeName index

typeCheckExpression :: (Comonad f, Traversable f, MonadError (TypeCheckError f) m) => f (Expression Unchecked f) -> TypeCheck f m (Expression Checked f)
typeCheckExpression expr = case extract expr of
  Literal literal -> pure $ Literal literal
  Reference _ identifier _ _ -> do
    value <- gets $ lookupVariable identifier . namespace
    maybe (typeCheckIdentifier $ identifier <$ expr) fromVariable $ snd <$> value
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

typeCheckIdentifier :: (Comonad f, Traversable f, MonadError (TypeCheckError f) m) => f Identifier -> TypeCheck f m (Expression Checked f)
typeCheckIdentifier variable = gets unchecked >>= \ns -> case Map.lookup (identifier $ extract variable) ns of
  Nothing -> throwError $ UnrecognisedVariable variable
  Just def -> referenceTo =<< extract <$> typeCheckDefinition def

typeCheckArg :: MonadError (TypeCheckError f) m => Arg Unchecked -> TypeCheck f m (Arg Checked)
typeCheckArg (Arg name _) = Arg name . Bound <$> newVar

unify :: (Comonad f, MonadError (TypeCheckError f) m)
 => f (Type Checked) -> f (Type Checked)
 -> m (Expression Checked f -> Expression Checked f, Expression Checked f -> Expression Checked f)
unify ty1 ty2 = go (extract ty1) (extract ty2) where
  go (Function _ f a) (Function _ g b) = do
    (h1, h2) <- unify (f <$ ty1) (g <$ ty2)
    (i1, i2) <- unify (a <$ ty1) (b <$ ty2)
    pure (i1 . h1, i2 . h2)
  go ty (Free name) = pure (id, bindType name ty)
  go (Free name) ty = pure (bindType name ty, id)
  go ty (Bound name) = pure (id, rebindType name ty)
  go (Bound name) ty = pure (rebindType name ty, id)
  go a b = when (a /= b) (throwError $ TypeMismatch ty1 ty2) *> pure (id, id)

newVar :: Monad m => TypeCheck f m Identifier
newVar = do
  (var, nextVar') <- genVar <$> gets (^. _nextVar)
  _nextVar .= nextVar'
  pure var

functionOf :: Type Checked -> Type Checked
functionOf a = Function UnknownArity a $ Free $ Identifier "_"

fromVariable :: (Comonad f, Monad m) => Variable f -> TypeCheck f m (Expression Checked f)
fromVariable (NS.Definition def) = referenceTo $ extract def
fromVariable (NS.Arg arg) = pure $ referenceArg $ extract arg

referenceArg :: Arg Checked -> Expression Checked f
referenceArg (Arg name ty) = Reference Local name ty ty

referenceTo :: (Comonad f, Monad m) => Definition Checked f -> TypeCheck f m (Expression Checked f)
referenceTo def = freeTypes newVar $ Reference Global (identifier def) (typeOf def) (typeOf def)

-- valueOf :: Comonad f => Definition Checked f -> Expression Checked f
-- valueOf (Definition _ value) = extract value
