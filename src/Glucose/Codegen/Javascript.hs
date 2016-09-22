module Glucose.Codegen.JavaScript (JSRaw, codegen, codegenDefinitions) where

import Control.Comonad
import Control.Lens
import Control.Monad.RWS
import Data.Foldable (toList)
import Data.Set as Set (Set, fromList, empty, insert, delete, member)
import Data.Text (Text, pack, unpack, intercalate)
import Glucose.Identifier
import Glucose.IR.Checked
import Glucose.VarGen

data JSRaw = JSRaw Text
instance Show JSRaw where
  show (JSRaw s) = unpack s

type Codegen = RWS () Text (Set Identifier, Set Identifier)

execCodegen :: Set Identifier -> Codegen a -> JSRaw
execCodegen vars m = JSRaw . snd $ evalRWS m () (empty, vars)

codegen :: Module -> JSRaw
codegen (Module defs) = codegenDefinitions $ map extract defs

codegenDefinitions :: [Definition] -> JSRaw
codegenDefinitions defs = execCodegen vars $ mapAttemptM_ attemptToDefine defs where
  vars = Set.fromList $ map identifier (toList defs)
  attemptToDefine def = maybe (pure False) ((True <$) . tell) =<< definition def

mapAttemptM_ :: Monad m => (a -> m Bool) -> [a] -> m ()
mapAttemptM_ _ [] = pure ()
mapAttemptM_ f as = deleteWhen f as >>= mapAttemptM_ f

deleteWhen :: Monad m => (a -> m Bool) -> [a] -> m [a]
deleteWhen _ [] = pure []
deleteWhen f as = go as where
  go [] = error "Recursive aliases in codegen?"
  go (a:as) = do
    shouldDelete <- f a
    if shouldDelete then pure as else (a:) <$> go as

definition :: Definition -> Codegen (Maybe Text)
definition (Definition (extract -> Identifier name) (extract -> Lambda args expr)) = do
  _2 %= delete (Identifier name)
  expr <- expression (extract expr)
  pure . Just $
    "function " <> name <> "(" <> intercalate ", " (map (arg.extract) args) <> ") {\n" <>
    "  return " <> expr <> "\n" <>
    "}\n"
definition (Definition (extract -> Identifier name) (extract -> Reference Global (Identifier target) _)) = do
  targetUndefined <- uses _2 (Set.member $ Identifier target)
  if targetUndefined
    then pure Nothing
    else do
      _2 %= delete (Identifier name)
      pure . Just $ name <> " = " <> target <> "\n"
definition (Definition (extract -> Identifier name) def) = do
  _2 %= delete (Identifier name)
  expr <- expression (extract def)
  pure . Just $ name <> " = " <> expr <> "\n"

expression :: Expression -> Codegen Text
expression (Literal a) = pure . pack $ show a
expression (Reference _ (Identifier a) _) = pure a
expression (Constructor (extract -> typeName) _) = do
  typeDefined <- uses _1 (Set.member typeName)
  unless typeDefined $ do
    _1 %= Set.insert typeName
    tell $ typeDefinition typeName
  pure $ "new " <> identify typeName <> "()"
expression (Lambda args expr) = do
  expr <- expression (extract expr)
  pure $
    "function(" <> intercalate ", " (map (arg . extract) args) <> ") {" <>
    " return " <> expr <> " " <>
    "}"
expression (Apply (extract -> f) (extract -> a)) = case flattenApply f a of
  Application root calls partial -> maybe fullApply partialApply partial where
    fullApply = foldl (<>) <$> expression root <*> traverse (fmap parenList . traverse expression) calls
    -- TODO: variable names can conflict with variables from outer scopes
    partialApply (Partial ty args) = do
      let residual = take (arity ty) variables
      args <- traverse expression args
      full <- fullApply
      pure $ "function" <> parenList residual <> " { return " <> full <> parenList (args ++ residual) <> " }"

typeDefinition :: Identifier -> Text
typeDefinition (Identifier typeName) = typeName <> " = function() {}\n"

arity :: Type -> Int
arity (Function (Arity n m) _ _) = n - m
arity _ = 0

-- TODO: need to build lambdas to coerce function arguments to the expected arity

arg :: Arg -> Text
arg (Arg (Identifier a) _) = a

parenList :: [Text] -> Text
parenList as = "(" <> intercalate ", " as <> ")"
