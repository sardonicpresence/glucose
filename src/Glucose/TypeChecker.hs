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
import Glucose.IR hiding (checked)
import Glucose.Namespace hiding (Arg, Definition)
import qualified Glucose.Namespace as NS
import Glucose.TypeChecker.TypeCheckError
import Glucose.TypeChecker.Unify
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

typeCheck :: (Comonad f, Traversable f, MonadError (TypeCheckError f) m) => Module Unchecked f -> m (Module Checked f)
typeCheck (Module defs) = evalStateT (typeCheckModule $ Module defs) =<< mkTypeChecker defs

typeCheckModule :: Module Unchecked f -> TypeCheck f m (Module Checked f)
typeCheckModule (Module defs) = fmap Module . for defs $ \def -> do
    existing <- uses namespace . lookupDefinition . identify $ extract def
    maybe (typeCheckDefinition def) pure existing

typeCheckDefinition :: f (Definition Unchecked f) -> TypeCheck f m (f (Definition Checked f))
typeCheckDefinition def = define (identify $ extract def) <=< for def $ \case
  Definition name value -> do
    recursive <- uses checking . elem $ extract name
    when recursive . throwError $ RecursiveDefinition name
    startChecking $ extract name
    nextVar .= mkVarGen
    Definition name <$> (traverse (bindTypes newVar) =<< typeCheckExpression value)
  Constructor name typeName index -> do
    let key = (extract typeName, index)
    modifyingM constructors . Map.insertOr key typeName $ duplicateDefinition typeName
    pure $ Constructor name typeName index

typeCheckExpression :: f (Expression Unchecked f) -> TypeCheck f m (f (Expression Checking f))
typeCheckExpression expr = for expr $ \case
  Literal literal -> pure $ Literal literal
  Reference _ identifier _ _ -> do
    referenced <- uses namespace $ fmap snd . lookupVariable identifier
    maybe (typeCheckIdentifier $ identifier <$ expr) referenceVariable referenced
  Lambda args def -> do
    typeCheckedArgs <- traverse (traverse typeCheckArg) args
    pushArgs typeCheckedArgs
    Lambda typeCheckedArgs <$> typeCheckExpression def
  Apply expr arg ty -> do
    expr' <- typeCheckExpression expr
    arg' <- typeCheckExpression arg
    ty' <- checkingType newVar ty
    unifier <- unify (typeOf <$> expr') (functionReturning ty' . typeOf <$> arg')
    pure $ Apply (expr' <&> typeAnnotations %~ unifier) (arg' <&> typeAnnotations %~ unifier) (unifier ty')

functionReturning :: Annotations ann => Type ann -> Type ann -> Type ann
functionReturning returnType argType = dataType # Function UnknownArity argType returnType

typeCheckIdentifier :: f Identifier -> TypeCheck f m (Expression Checking f)
typeCheckIdentifier variable = do
  referenced <- uses unchecked . Map.lookup $ extract variable
  maybe (throwError $ UnrecognisedVariable variable) (referenceTo . extract <=< typeCheckDefinition) referenced

typeCheckArg :: Arg Unchecked -> TypeCheck f m (Arg Checking)
typeCheckArg (Arg name _) = Arg name . Bound . Polymorphic <$> newVar

startChecking :: Identifier -> TypeCheck f m ()
startChecking name = checking %= Set.insert name

checked :: Identifier -> TypeCheck f m ()
checked name = do
  checking %= Set.delete name
  unchecked %= Map.delete name

pushArgs :: [f (Arg Checking)] -> TypeCheck f m ()
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

referenceVariable :: Variable f -> TypeCheck f m (Expression Checking f)
referenceVariable (NS.Definition def) = referenceTo $ extract def
referenceVariable (NS.Arg arg) = pure . referenceArg $ extract arg

referenceTo :: Definition Checked f -> TypeCheck f m (Expression Checking f)
referenceTo def = freeTypes newVar $ Reference Global (extract $ identifier def) (typeOf def) (typeOf def)

referenceArg :: Arg Checking -> Expression Checking f
referenceArg (Arg name ty) = Reference Local name ty ty
