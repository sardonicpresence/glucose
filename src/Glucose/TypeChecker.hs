{-# LANGUAGE TemplateHaskell #-}
module Glucose.TypeChecker where

import Control.Comonad
import Control.Lens
import Control.Lens.TH ()
import Control.Lens.Utils
import Control.Monad.Except
import Control.Monad.State
import Data.Map.Strict as Map
import Data.Map.Utils as Map
import Data.Set as Set
import Data.Traversable

import Glucose.Desugar
import Glucose.Identifier
import Glucose.IR
-- import Glucose.IR.Checked (bindTypes, bindType, rebindType, freeTypes)
import Glucose.Namespace hiding (Arg, Definition)
import qualified Glucose.Namespace as NS
import Glucose.TypeChecker.TypeCheckError
import Glucose.VarGen

data TypeChecker f = TypeChecker
  { _namespace :: Namespace f
  , _constructors :: Map (Identifier, Int) (f Identifier)
  , _checking :: Set Identifier
  , _nextVar :: VarGen
  , _unchecked :: Map Identifier (Desugared f Definition) }

makeLenses ''TypeChecker

mkTypeChecker :: (Comonad f, MonadError (TypeCheckError f) m) => [Desugared f Definition] -> m (TypeChecker f)
mkTypeChecker defs = do
  unchecked <- bindings duplicateDefinition (identify . extract) defs
  pure $ TypeChecker emptyNamespace Map.empty Set.empty mkVarGen unchecked

type TypeCheck f m a = (Comonad f, Traversable f, MonadError (TypeCheckError f) m) => StateT (TypeChecker f) m a

typeCheck :: (Comonad f, Traversable f, MonadError (TypeCheckError f) m) => Module Untyped f -> m (Module Checked f)
typeCheck (Module defs) = evalStateT (typeCheckModule $ Module defs) =<< mkTypeChecker defs

typeCheckModule :: Module Untyped f -> TypeCheck f m (Module Checked f)
typeCheckModule (Module defs) = fmap Module . for defs $ \def -> do
    existing <- uses namespace . lookupDefinition . identify $ extract def
    maybe (typeCheckDefinition def) pure existing

typeCheckDefinition :: f (Definition Untyped f) -> TypeCheck f m (f (Definition Checked f))
typeCheckDefinition def = define (identify $ extract def) <=< for def $ \case
  Definition name value -> do
    recursive <- uses checking . elem $ extract name
    when recursive . throwError $ RecursiveDefinition name
    startChecking $ extract name
    -- nextVar .= mkVarGen
    Definition name <$> typeCheckExpression value
  Constructor name typeName index -> do
    let key = (extract typeName, index)
    modifyingM constructors . Map.insertOr key typeName $ duplicateDefinition typeName
    pure $ Constructor name typeName index

typeCheckExpression :: f (Expression Untyped f) -> TypeCheck f m (f (Expression Checked f))
typeCheckExpression expr = for expr $ \case
  Literal literal -> pure $ Literal literal
  Reference _ identifier _ _ -> do
    referenced <- uses namespace $ fmap snd . lookupVariable identifier
    maybe (typeCheckIdentifier $ identifier <$ expr) referenceVariable referenced
  Lambda args def -> do
    typeCheckedArgs <- traverse (traverse typeCheckArg) args
    pushArgs typeCheckedArgs
    retType <- Bound <$> newVar
    def' <- fmap (types %~ uncheckedType) <$> typeCheckExpression def
    def'' <- (<$> def') . snd <$> unify (typeOf <$> def') (retType <$ def)
    def''' <- traverse (types $ checkedType newVar) def''
    pure $ Lambda typeCheckedArgs def'''
  Apply expr arg ty -> do
    expr' <- fmap (types %~ uncheckedType) <$> typeCheckExpression expr
    arg' <- fmap (types %~ uncheckedType) <$> typeCheckExpression arg
    (f, g) <- unify (typeOf <$> expr') (Function UnknownArity . typeOf <$> arg' <*> pure ty)
    pure $ Apply (f <$> expr') (g <$> arg')


functionOf :: Type Checked -> Type Checked
functionOf a = Function UnknownArity a . Free $ Identifier "_"

typeCheckIdentifier :: f Identifier -> TypeCheck f m (Expression Checked f)
typeCheckIdentifier variable = do
  referenced <- uses unchecked . Map.lookup $ extract variable
  maybe (throwError $ UnrecognisedVariable variable) (referenceTo . extract <=< typeCheckDefinition) referenced

typeCheckArg :: Arg Untyped -> TypeCheck f m (Arg Checked)
typeCheckArg (Arg name _) = Arg name . Polymorphic <$> newVar

unifyUnchecked :: (Comonad f, MonadError (TypeCheckError f) m)
 => f (Type Unchecked) -> f (Type Unchecked)
 -> m (Expression Unchecked f -> Expression Unchecked f, Expression Unchecked f -> Expression Unchecked f)
unifyUnchecked ty1 ty2 = go (extract ty1) (extract ty2) where
  go ty (Free name) = pure (id, replaceType (Free name) ty)
  go (Free name) ty = pure (replaceType (Free name) ty, id)
  go (Bound a) (Bound b) = bimap _Bound _Bound $ unify a b
  -- go (Bound (Function _ f a)) (Bound (Function _ g b)) = do
  --   (h1, h2) <- unifyUnchecked (f <$ ty1) (g <$ ty2)
  --   (i1, i2) <- unifyUnchecked (a <$ ty1) (b <$ ty2)
  --   pure (i1 . h1, i2 . h2)
  -- go ty (Bound (Polymorphic name)) = pure (id, replaceType (Bound (Polymorphic name)) ty)
  -- go (Bound (Polymorphic name)) ty = pure (replaceType (Bound (Polymorphic name)) ty, id)
  -- go a b | a /= b = throwError $ TypeMismatch ty1 ty2
  -- go _ _ = pure (id, id)

unifyChecked :: (Comonad f, MonadError (TypeCheckError f) m)
 => f (Type Checked) -> f (Type Checked)
 -> m (Expression Checked f -> Expression Checked f, Expression Checked f -> Expression Checked f)
unifyChecked ty1 ty2 = go (extract ty1) (extract ty2) where
  go (Known (Function _ f a)) (Known (Function _ g b)) = do
    (h1, h2) <- unifyChecked (f <$ ty1) (g <$ ty2)
    (i1, i2) <- unifyChecked (a <$ ty1) (b <$ ty2)
    pure (i1 . h1, i2 . h2)
  go ty (Known (Polymorphic name)) = pure (id, replaceType (Known (Polymorphic name)) ty)
  go (Known (Polymorphic name)) ty = pure (replaceType (Known (Polymorphic name)) ty, id)
  go a b | a /= b = throwError $ TypeMismatch (uncheckedType <$> ty1) (uncheckedType <$> ty2)
  go _ _ = pure (id, id)

replace :: Type ann -> Type ann -> Type ann -> Type ann
replace from to a | a == from = to
replace _ _ a = a

-- unify :: (Comonad f, MonadError (TypeCheckError f) m)
--  => f (DataType t) -> f (DataType t)
--  -> m (Expression Unchecked f -> Expression Unchecked f, Expression Unchecked f -> Expression Unchecked f)
unify' ty1 ty2 = go (extract ty1) (extract ty2) where
  go (Function _ f a) (Function _ g b) = do
    (h1, h2) <- unify (f <$ ty1) (g <$ ty2)
    (i1, i2) <- unify (a <$ ty1) (b <$ ty2)
    pure (i1 . h1, i2 . h2)
  go a b@Polymorphic{} = pure (id, replace b a)
  go a@Polymorphic{} ty = pure (replace a b, id)
  go a b | a /= b = throwError $ TypeMismatch ty1 ty2
  go _ _ = pure (id, id)

startChecking :: Identifier -> TypeCheck f m ()
startChecking name = checking %= Set.insert name

checked :: Identifier -> TypeCheck f m ()
checked name = do
  checking %= Set.delete name
  unchecked %= Map.delete name

pushArgs :: [f (Arg Checked)] -> TypeCheck f m ()
pushArgs args = modifyingM namespace $ \ns ->
  foldM (flip $ handlingDuplicates declareArg) (pushScope ns) args

popArgs :: TypeCheck f m ()
popArgs = namespace %= popScope

define :: Identifier -> f (Definition Checked f) -> TypeCheck f m (f (Definition Checked f))
define name value = do
  modifyingM namespace $ handlingDuplicates declareDefinition value
  checked name
  pure value

handlingDuplicates :: (Comonad f, Bound f (f a), MonadError (TypeCheckError f) m)
 => (f a -> Namespace f -> Either (Variable f) (Namespace f)) -> f a -> Namespace f -> m (Namespace f)
handlingDuplicates declarer value = either (duplicateDefinition value) pure . declarer value

duplicateDefinition :: (Functor f, Bound f a, Bound f b, MonadError (TypeCheckError f) m) => a -> b -> m c
duplicateDefinition new = throwError . DuplicateDefinition (identifier new) . void . identifier

newVar :: TypeCheck f m Identifier
newVar = nextVar %%= genVar

referenceVariable :: Variable f -> TypeCheck f m (Expression Checked f)
referenceVariable (NS.Definition def) = referenceTo $ extract def
referenceVariable (NS.Arg arg) = pure . referenceArg $ extract arg

referenceTo :: Definition Checked f -> TypeCheck f m (Expression Checked f)
referenceTo def = freeTypes newVar $ Reference Global (extract $ identifier def) (typeOf def) (typeOf def)

referenceArg :: Arg Checked -> Expression Checked f
referenceArg (Arg name ty) = Reference Local name ty ty
