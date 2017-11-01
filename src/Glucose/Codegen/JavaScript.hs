module Glucose.Codegen.JavaScript (JavaScript, codegen, codegenModuleDefinitions, codegenModule, codegenDefinitions) where

import Control.Comonad
import Control.Lens
import Control.Lens.TH ()
import Control.Monad.RWS
import Data.Set as Set (Set, fromList, empty, insert, delete, member)
import Data.Text as Text (Text, pack)
import Glucose.Identifier
import Glucose.IR.Checked
import JavaScript.AST (JavaScript(..))
import qualified JavaScript.AST as JS
import qualified JavaScript.Name as JS

-- * Compiler interface

codegen :: Comonad f => Module f -> Text
codegen = pack . show . codegenModule

codegenModuleDefinitions :: Comonad f => Module f -> Text
codegenModuleDefinitions = codegen

-- * Test interface

codegenModule :: Comonad f => Module f -> JavaScript
codegenModule (Module defs) = codegenDefinitions $ map extract defs

codegenDefinitions :: Comonad f => [Definition f] -> JavaScript
codegenDefinitions defs = execCodegen names $ mapAttemptM_ attemptToDefine defs where
  names = fromList $ map identify defs
  attemptToDefine def = maybe (pure False) ((True <$) . tell . pure) =<< definition def

-- * Codegen monad

data Codegenerator = Codegenerator { _undeclared :: Set Identifier, _declaredTypes :: Set Identifier }

undeclared :: Lens' Codegenerator (Set Identifier)
undeclared = lens _undeclared $ \a b -> a { _undeclared = b }

declaredTypes :: Lens' Codegenerator (Set Identifier)
declaredTypes = lens _declaredTypes $ \a b -> a { _declaredTypes = b }

type Codegen = RWS () [JS.Definition] Codegenerator

execCodegen :: Set Identifier -> Codegen a -> JavaScript
execCodegen names m = JavaScript . snd . evalRWS m () $ Codegenerator names empty

{- | Perform a potentially unsuccessful operation on each element in a list,
     doing so in the provided order to the extent possible.
     Errors if it is not possible to successfully operate on all elements. -}
mapAttemptM_ :: Monad m => (a -> m Bool) -> [a] -> m ()
mapAttemptM_ _ [] = pure ()
mapAttemptM_ f as = deleteWhen f as >>= mapAttemptM_ f

{- | Perform an operation on each element in a list until one succeeds, returning the list with that element removed.
     Errors if any operation failed and none succeeded, to prevent infinite recursion. -}
deleteWhen :: Monad m => (a -> m Bool) -> [a] -> m [a]
deleteWhen _ [] = pure []
deleteWhen f as = go as where
  go [] = error "Recursive aliases in codegen!"
  go (a:as) = do
    shouldDelete <- f a
    if shouldDelete then pure as else (a:) <$> go as

-- * Internals

definition :: Comonad f => Definition f -> Codegen (Maybe JS.Definition)
definition (Definition (extract -> name) def) = case extract def of
  Lambda arg expr -> do
    expr <- expression (extract expr)
    undeclared %= delete name
    pure . Just $ JS.Function (mkName name) (namesOf [arg]) (Just expr)
  def -> do
    canDefineYet <- case def of
      Reference (Global target) _ -> uses undeclared $ not . member target
      _ -> pure True
    if not canDefineYet then pure Nothing else do
      expr <- expression def
      undeclared %= delete name
      pure . Just $ JS.Assign (var name) expr
definition (Constructor (extract -> name) (extract -> typeName) _) = do
  typeDefined <- uses declaredTypes $ member typeName
  unless typeDefined $ do
    declaredTypes %= insert typeName
    tell [typeDefinition typeName]
  undeclared %= delete name
  pure . Just $ JS.Assign (var name) $ JS.New (referenceTo typeName) []

typeDefinition :: Identifier -> JS.Definition
typeDefinition typeName = JS.Assign (var typeName) (JS.Lambda [] Nothing)

expression :: Comonad f => Expression f -> Codegen JS.Expression
expression (Literal (IntegerLiteral a)) = pure $ JS.IntegerLiteral a
expression (Literal (FloatLiteral a)) = pure $ JS.FloatLiteral a
expression (Reference (extract -> a) _) = pure $ referenceTo a
expression (Lambda arg expr) = do
  expr <- expression (extract expr)
  pure $ JS.Lambda (namesOf [arg]) (Just expr)
expression (Apply (extract -> f) (extract -> x) _) = do
  let coerceArgs = traverse $ uncurry coerce
  uncurry (callChain coerceArgs) $ flatten f x

{- | Coerces functions to an expected arity by wrapping with a lambda if required. -}
coerce :: Comonad f => Type -> Expression f -> Codegen JS.Expression
coerce ty expr = if from == to then expression expr else lambda to $ \args -> callChain call expr args where
  from = effectiveArity $ typeOf expr
  to = effectiveArity ty
  call = pure . map (referenceTo . snd)

callChain :: Comonad f => (Call a -> Codegen [JS.Expression]) -> Expression f -> [a] -> Codegen JS.Expression
callChain call f as = do
  let (Application _ calls partial) = groupApplication (typeOf f) as
  fullApplications <- foldl JS.Call <$> expression f <*> traverse call calls
  maybe (pure fullApplications) (partialApplication call fullApplications) partial

partialApplication :: Monad f => (Call a -> f [JS.Expression]) -> JS.Expression -> Partial a -> f JS.Expression
partialApplication call f (Partial arity as) = call as >>= \as ->
  lambda arity $ \args -> pure $ JS.Call f (as ++ map referenceTo args)

lambda :: Functor f => Int -> ([Identifier] -> f JS.Expression) -> f JS.Expression
lambda arity f = let args = map positional [1..arity] in JS.Lambda (namesOf args) . Just <$> f args

-- * Utilities

positional :: Int -> Identifier
positional = Identifier . pack . ("$" <>) . show

namesOf :: (Comonad f, Bound f a) => [a] -> [JS.Name]
namesOf = map (mkName . identify)

referenceTo :: Identifier -> JS.Expression
referenceTo = JS.Reference . var

var :: Identifier -> JS.Identifier
var name = let (JS.Name mangled) = mkName name in JS.Identifier [mangled]

mkName :: Identifier -> JS.Name
mkName (Identifier name) = JS.mkName name
