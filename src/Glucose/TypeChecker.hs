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

data TypeChecker = TypeChecker
  { namespace :: Namespace
  , checking :: Set Identifier
  , unchecked :: Map Identifier (FromSource AST.Definition) }

_namespace :: Lens TypeChecker TypeChecker Namespace Namespace
_namespace = lens namespace (\a b -> a { namespace = b })

_checking :: Lens TypeChecker TypeChecker (Set Identifier) (Set Identifier)
_checking = lens checking (\a b -> a { checking = b })

mkTypeChecker :: Error m => [FromSource AST.Definition] -> m TypeChecker
mkTypeChecker defs = TypeChecker emptyNamespace Set.empty <$> toMap defs where
  toMap = foldM insertInto Map.empty
  insertInto m def = insertOrError m (identifier def) def $
    duplicateDefinition (startLocation def) (identifier def) . startLocation
  insertOrError m k v formatError = case Map.insertLookupWithKey noReplace k v m of
    (Nothing, m') -> pure m'
    (Just existing, _) -> formatError existing
  noReplace _ _ a = a

push :: Error m => Identifier -> TypeCheck m ()
push name = _checking %= Set.insert name

popArgs :: Error m => TypeCheck m ()
popArgs = _namespace %= popScope

define :: Error m => Identifier -> FromSource IR.Definition -> TypeCheck m (FromSource IR.Definition)
define name value = do
  tc@TypeChecker{namespace,unchecked,checking} <- get
  namespace' <- declare (Definition value) namespace
  put tc { namespace = namespace', unchecked = Map.delete name unchecked, checking = Set.delete name checking }
  pure value

type TypeCheck m a = StateT TypeChecker m a

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
    IR.Definition name <$> traverse typeCheckExpression (duplicate value)

typeCheckExpression :: Error m => FromSource AST.Expression -> TypeCheck m IR.Expression
typeCheckExpression expr = case extract expr of
  AST.Literal (AST.IntegerLiteral a) -> pure $ IR.Literal (IR.IntegerLiteral a)
  AST.Literal (AST.FloatLiteral a) -> pure $ IR.Literal (IR.FloatLiteral a)
  AST.Variable identifier -> do
    value <- gets $ lookupVariable identifier . namespace
    maybe (typeCheckIdentifier $ identifier <$ expr) (pure . fromVariable) $ snd <$> value

typeCheckIdentifier :: Error m => FromSource Identifier -> TypeCheck m IR.Expression
typeCheckIdentifier variable = gets unchecked >>= \ns -> case Map.lookup (identifier variable) ns of
  Nothing -> unrecognisedVariable (startLocation variable) (identifier variable)
  Just def -> referenceTo . extract <$> typeCheckDefinition def

fromVariable :: Variable -> IR.Expression
fromVariable (Definition def) = referenceTo $ extract def

referenceTo :: IR.Definition -> IR.Expression
referenceTo def = IR.Reference IR.Global (identifier def) (IR.typeOf def)

valueOf :: IR.Definition -> IR.Expression
valueOf (IR.Definition _ value) = extract value
