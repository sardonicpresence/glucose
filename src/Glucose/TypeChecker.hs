module Glucose.TypeChecker where

import Control.Comonad
import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Map.Strict as Map
import Data.Set as Set

import Glucose.Error
import Glucose.Identifier
import Glucose.IR
import Glucose.Lexer.Location
import Glucose.Namespace
import Glucose.Parser.Source

data TypeChecker = TypeChecker
  { namespace :: Namespace
  , constructors :: Map (Identifier, Int) Location
  , checking :: Set Identifier
  , unchecked :: Map Identifier (FromSource (Definition Unchecked)) }

_namespace :: Lens' TypeChecker Namespace
_namespace = lens namespace (\a b -> a { namespace = b })

_constructors :: Lens' TypeChecker (Map (Identifier, Int) Location)
_constructors = lens constructors (\a b -> a { constructors = b })

_checking :: Lens' TypeChecker (Set Identifier)
_checking = lens checking (\a b -> a { checking = b })

mkTypeChecker :: Error m => [FromSource (Definition Unchecked)] -> m TypeChecker
mkTypeChecker defs = TypeChecker emptyNamespace Map.empty Set.empty <$> toMap defs where
  toMap = foldM insertInto Map.empty
  insertInto m def = insertOr (identifier def) def (formatError def) m
  formatError def = duplicateDefinition (startLocation def) (identifier def) . startLocation

push :: Error m => Identifier -> TypeCheck m ()
push name = _checking %= Set.insert name

popArgs :: Error m => TypeCheck m ()
popArgs = _namespace %= popScope

define :: Error m => Identifier -> FromSource (Definition Checked) -> TypeCheck m (FromSource (Definition Checked))
define name value = do
  tc@TypeChecker{namespace,unchecked,checking} <- get
  namespace' <- declare value namespace
  put tc { namespace = namespace', unchecked = Map.delete name unchecked, checking = Set.delete name checking }
  pure value

type TypeCheck m a = StateT TypeChecker m a

typeCheck :: Error m => Module Unchecked -> m (Module Checked)
typeCheck (Module defs) = (Module <$>) . evalStateT (traverse go defs) =<< mkTypeChecker defs where
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
    Definition name <$> traverse typeCheckExpression (duplicate value)

typeCheckExpression :: Error m => FromSource (Expression Unchecked) -> TypeCheck m (Expression Checked)
typeCheckExpression expr = case extract expr of
  Literal literal -> pure $ Literal literal
  Reference _ identifier _ -> do
    value <- gets $ lookupVariable identifier . namespace
    maybe (typeCheckIdentifier $ identifier <$ expr) (pure . fromVariable) $ snd <$> value
  Constructor typeName index -> do
    let key = (extract typeName, index)
    let formatError = duplicateDefinition (startLocation typeName) (extract typeName)
    m <- gets constructors
    m' <- insertOr key (startLocation typeName) formatError m
    _constructors .= m'
    pure $ Constructor typeName index

typeCheckIdentifier :: Error m => FromSource Identifier -> TypeCheck m (Expression Checked)
typeCheckIdentifier variable = gets unchecked >>= \ns -> case Map.lookup (identifier variable) ns of
  Nothing -> unrecognisedVariable (startLocation variable) (identifier variable)
  Just def -> referenceTo . extract <$> typeCheckDefinition def

insertOr :: (Applicative f, Ord k) => k -> v -> (v -> f (Map k v)) -> Map k v -> f (Map k v)
insertOr k v onExisting m =
  case Map.insertLookupWithKey noReplace k v m of
    (Nothing, m') -> pure m'
    (Just existing, _) -> onExisting existing
  where noReplace _ _ a = a

fromVariable :: FromSource (Definition Checked) -> Expression Checked
fromVariable = referenceTo . extract

referenceTo :: Definition Checked -> Expression Checked
referenceTo def = Reference Global (identifier def) (typeOf def)

valueOf :: Definition Checked -> Expression Checked
valueOf (Definition _ value) = extract value
