{-# LANGUAGE TemplateHaskell #-}
module Glucose.TypeChecker where

import Control.Comonad
import Control.Lens
import Control.Lens.TH ()
import Control.Lens.Utils
import Control.Monad.Except
import Control.Monad.State
import Data.List as List
import Data.Map.Strict as Map hiding (mapMaybe)
import Data.Map.Utils as Map
import Data.Maybe
import Data.Ord (comparing)
import Data.Set as Set
import Data.Traversable

import Glucose.Desugar
import Glucose.Identifier
import Glucose.IR
import Glucose.Namespace hiding (Arg, Definition)
import qualified Glucose.Namespace as NS
import Glucose.TypeChecker.TypeCheckError
import Glucose.TypeChecker.Unify
import Glucose.Unique
import Glucose.VarGen

data TypeChecker f = TypeChecker
  { _namespace :: Namespace f
  , _constructors :: Map (Identifier, Int) (f Identifier)
  , _checking :: Set Identifier
  , _lastUnique :: Unique
  , _unchecked :: Map Identifier (Desugared f Definition) }

makeLenses ''TypeChecker

mkTypeChecker :: (Comonad f, MonadError (TypeCheckError f) m) => [Desugared f Definition] -> m (TypeChecker f)
mkTypeChecker defs = do
  unchecked <- bindings duplicateDefinition (identify . extract) defs
  pure $ TypeChecker emptyNamespace Map.empty Set.empty mkUnique unchecked

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
    checked <- typeCheckExpression value
    let finaliseTypes = flip evalState mkVarGen . bindTypes (state genVar) . unboxReturnType
    pure $ Definition name (finaliseTypes <$> checked)
  Constructor name typeName index -> do
    let key = (extract typeName, index)
    modifyingM constructors . Map.insertOr key typeName $ duplicateDefinition typeName
    pure $ Constructor name typeName index

typeCheckExpression :: f (Expression Unchecked f) -> TypeCheck f m (f (Expression Checking f))
typeCheckExpression expr = for expr $ \case
  Literal literal -> pure $ Literal literal
  Reference (extract -> identifier) _ -> do
    referenced <- uses namespace $ fmap snd . lookupVariable identifier
    maybe (typeCheckIdentifier $ identifier <$ expr) referenceVariable referenced
  Lambda arg def -> do
    let checkedArg = evalState (traverse (typeCheckArg $ state genVar) arg) mkVarGen
    pushArgs $ pure checkedArg
    typeCheckedExpr <- notLambda =<< typeCheckExpression def
    [typeCheckedArg] <- popArgs
    pure $ Lambda typeCheckedArg typeCheckedExpr
  Apply expr arg ty -> do
    expr' <- notLambda =<< typeCheckExpression expr
    arg' <- notLambda =<< typeCheckExpression arg
    ty' <- checkingType newVar ty
    unifier <- unifyApply (typeOf <$> expr') (typeOf <$> arg') ty'
    namespace . declaredArgs . traversed . argType %= unifier
    pure $ Apply (expr' <&> typeAnnotations %~ unifier) (arg' <&> typeAnnotations %~ unifier) (unifier ty')

unifyApply :: (Comonad f, Traversable f, MonadError (TypeCheckError f) m)
 => f (Type Checking) -> f (Type Checking) -> Type Checking -> m (Type Checking -> Type Checking)
unifyApply fn arg returnType  = case extract fn of
  BoundType (Function _ a b) -> do
    f <- unify a arg
    g <- unify (f returnType) (f b <$ fn)
    pure $ g . f
  _ -> unify (functionReturning returnType $ extract arg) fn

functionReturning :: Annotations ann => Type ann -> Type ann -> Type ann
-- TODO: Only (Arity 1) while we only support single-argument functions
functionReturning returnType argType = dataType # Function (Arity 1) argType returnType

unboxReturnType :: (Functor f, Annotations ann) => Expression ann f -> Expression ann f
unboxReturnType (Lambda arg def) = Lambda arg $ unboxApply <$> def where
  unboxApply (Apply f a ty) = Apply f a $ ty & dataType %~ unboxed
  unboxApply a = a
unboxReturnType a = a

notLambda :: f (Expression Checking f) -> TypeCheck f m (f (Expression Checking f))
notLambda expr = case extract expr of
  Lambda{} -> throwError (LocalLambda $ void expr)
  _ -> pure expr

typeCheckIdentifier :: f Identifier -> TypeCheck f m (Expression Checking f)
typeCheckIdentifier variable = do
  referenced <- uses unchecked . Map.lookup $ extract variable
  maybe (throwError $ UnrecognisedVariable variable) (referenceTo . extract <=< typeCheckDefinition) referenced

typeCheckArg :: Functor f => f Identifier -> Arg Unchecked -> f (Arg Checking)
typeCheckArg newVar (Arg name _) = Arg name . Type . Bound . Polymorphic <$> newVar

startChecking :: Identifier -> TypeCheck f m ()
startChecking name = checking %= Set.insert name

checked :: Identifier -> TypeCheck f m ()
checked name = do
  checking %= Set.delete name
  unchecked %= Map.delete name

pushArgs :: [f (Arg Checking)] -> TypeCheck f m ()
pushArgs args = modifyingM namespace $ \ns ->
  ifoldlM (flip . handlingDuplicates . declareArg) (pushScope ns) args

popArgs :: TypeCheck f m [f (Arg Checking)]
popArgs = do
  ns <- use namespace
  let (vars, ns') = popScope ns
  namespace .= ns'
  let args = mapMaybe (\case NS.Arg i arg -> Just (i, arg); _ -> Nothing) vars
  pure . List.map snd $ sortBy (comparing fst) args

define :: Identifier -> f (Definition Checked f) -> TypeCheck f m (f (Definition Checked f))
define name value = do
  when (isCAF value) $ throwError (CAF $ void value)
  modifyingM namespace $ handlingDuplicates declareDefinition value
  checked name
  pure value

isCAF :: Comonad f => f (Definition Checked f) -> Bool
isCAF (extract -> Definition _ (extract -> Apply{})) = True
isCAF _ = False

handlingDuplicates :: (Comonad f, Bound f (f a), MonadError (TypeCheckError f) m)
 => (f a -> Namespace f -> Either (Variable f) (Namespace f)) -> f a -> Namespace f -> m (Namespace f)
handlingDuplicates declarer value = either (duplicateDefinition value) pure . declarer value

duplicateDefinition :: (Functor f, Bound f a, Bound f b, MonadError (TypeCheckError f) m) => a -> b -> m c
duplicateDefinition new = throwError . DuplicateDefinition (identifier new) . void . identifier

newVar :: TypeCheck f m Unique
newVar = lastUnique <%= nextUnique

referenceVariable :: Variable f -> TypeCheck f m (Expression Checking f)
referenceVariable (NS.Definition def) = referenceTo $ extract def
referenceVariable (NS.Arg _ arg) = pure . referenceArg $ extract arg

referenceTo :: Definition Checked f -> TypeCheck f m (Expression Checking f)
referenceTo def = freeTypes newVar $ Reference (Global . extract $ identifier def) (typeOf def)

referenceArg :: Arg Checking -> Expression Checking f
referenceArg (Arg name ty) = Reference (Local name) ty

checkingType :: Applicative f => f Unique -> Type Unchecked -> f (Type Checking)
checkingType = typeVariables . const . (Any <$>)
