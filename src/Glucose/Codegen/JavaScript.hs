module Glucose.Codegen.JavaScript (JavaScript, codegen, codegenModuleDefinitions, codegenModule, codegenDefinitions) where

import Control.Comonad
import Control.Lens
import Control.Lens.TH ()
import Control.Monad.RWS
import Control.Monad.State
import Data.Set as Set (Set, fromList, empty, insert, delete, member)
import Data.Text as Text (Text, pack, intercalate, concat)
import Glucose.Identifier
import Glucose.IR.Checked
import Glucose.VarGen
import JavaScript.AST

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
  attemptToDefine def = maybe (pure False) ((True <$) . tell) =<< definition def

-- * Codegen monad

data Codegenerator = Codegenerator { _undeclared :: Set Identifier, _declaredTypes :: Set Identifier }

undeclared :: Lens' Codegenerator (Set Identifier)
undeclared = lens _undeclared $ \a b -> a { _undeclared = b }

declaredTypes :: Lens' Codegenerator (Set Identifier)
declaredTypes = lens _declaredTypes $ \a b -> a { _declaredTypes = b }

type Codegen = RWS () Text Codegenerator

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

definition :: Comonad f => Definition f -> Codegen (Maybe Text)
definition (Definition (extract -> name) def) = case extract def of
  Lambda args expr -> do
    expr <- expression (extract expr)
    undeclared %= delete name
    -- Handle declarations of functions with reserved names
    let decl = case name of Identifier n -> if isReserved n then var name <> " = function" else "function " <> var name
    pure . Just $
      decl <> argList args <> " {\n" <>
      "  return " <> expr <> "\n" <>
      "}\n"
  def -> do
    canDefineYet <- case def of
      Reference Global target _ _ -> uses undeclared $ not . member target
      _ -> pure True
    if not canDefineYet then pure Nothing else do
      expr <- expression def
      undeclared %= delete name
      pure . Just $ var name <> " = " <> expr <> "\n"
definition (Constructor (extract -> name) (extract -> typeName) _) = do
  typeDefined <- uses declaredTypes $ member typeName
  unless typeDefined $ do
    declaredTypes %= insert typeName
    tell $ typeDefinition typeName
  undeclared %= delete name
  pure . Just $ var name <> " = " <> "new " <> var typeName <> "()\n"

expression :: Comonad f => Expression f -> Codegen Text
expression (Literal a) = pure . pack $ show a
expression (Reference _ a _ _) = pure $ var a
expression (Lambda args expr) = do
  expr <- expression (extract expr)
  pure $ "function" <> argList args <> " { return " <> expr <> " }"
expression (Apply (extract -> expr) (extract -> args) _) = let (f, as) = flatten expr args in
  foldl (<>) <$> expression f <*> (map parenList <$> groupArgs coerce (typeOf f) as)

groupArgs :: (Type -> a -> Codegen Text) -> Type -> [a] -> Codegen [[Text]]
groupArgs genWithType ty = go ty [] where
  go _ [] [] = pure []
  go (CheckedType (Function arity a b)) as (r:rs) = do
    r' <- genWithType a r
    let as' = as ++ [r']
    case arity of
      Arity 1 -> (as' :) <$> go b [] rs
      _ -> go b as' rs
  go _ as [] = pure [as]
  go ty _ bs = error $ "Cannot apply " <> show (length bs) <> " arguments to expression of type " <> show ty

coerce :: Comonad f => Type -> Expression f -> Codegen Text
coerce ty expr = expression expr >>= \arg -> if from == to then pure arg else wrap to arg where
  wrap :: Int -> Text -> Codegen Text
  wrap arity arg = do
    let args = take arity variables -- TODO: avoid shadowing existing variables
    call <- Text.concat . map parenList <$> groupArgs (const pure) (typeOf expr) args
    pure $ "function" <> parenList args <> " { return " <> arg <> call <> " }"
  from = effectiveArity (typeOf expr)
  to = effectiveArity ty

effectiveArity :: Type -> Int
effectiveArity (CheckedType (Function (Arity n) _ _)) = n
effectiveArity (CheckedType (Function _ _ b)) = 1 + effectiveArity b
effectiveArity _ = 0

{-
expression (Apply (extract -> f) (extract -> a) _) = case flattenApply f a of
  -- TODO: need to build lambdas to coerce function arguments to the expected arity
  Application _ root calls partial -> maybe fullApply partialApply partial where
    fullApply = foldl (<>) <$> expression root <*> traverse (fmap parenList . traverse expression) calls
    -- TODO: variable names can conflict with variables from outer scopes
    partialApply (Partial ty args) = do
      let residual = take (arity ty) variables
      args <- traverse expression args
      full <- fullApply
      pure $ "function" <> parenList residual <> " { return " <> full <> parenList (args ++ residual) <> " }"
-}

typeDefinition :: Identifier -> Text
typeDefinition typeName = var typeName <> " = function() {}\n"

argList :: Comonad f => [f Arg] -> Text
argList = parenList . map (arg . extract) where
  arg (Arg a _) = var a

var :: Identifier -> Text
var (Identifier name) = if isReserved name
  then "global." <> name
  else name

-- * Utilities

arity :: Type -> Int
arity (CheckedType (Function (Arity n) _ _)) = n
arity _ = 0

parenList :: [Text] -> Text
parenList as = "(" <> intercalate ", " as <> ")"
