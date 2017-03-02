module Glucose.Codegen.JavaScript (codegen) where

import Control.Comonad
import Control.Lens
import Control.Monad.RWS
import Data.Foldable (toList)
import Data.Set as Set (Set, fromList, empty, insert, delete, member)
import Data.Text (Text, pack, unpack)
import Glucose.Identifier
import Glucose.IR

data JSRaw = JSRaw Text
instance Show JSRaw where
  show (JSRaw s) = unpack s

type Codegen = RWS () Text (Set Identifier, Set Identifier)

execCodegen :: Set Identifier -> Codegen a -> JSRaw
execCodegen vars m = JSRaw . snd $ evalRWS m () (empty, vars)

codegen :: Module Checked -> JSRaw
codegen (Module defs) = execCodegen vars $ mapAttemptM_ attemptToDefine (map extract defs) where
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

definition :: Definition Checked -> Codegen (Maybe Text)
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

expression :: Expression Checked -> Codegen Text
expression (Literal a) = pure . pack $ show a
expression (Reference _ (Identifier a) _) = pure a
expression (Constructor (extract -> typeName) _) = do
  typeDefined <- uses _1 (Set.member typeName)
  unless typeDefined $ do
    _1 %= Set.insert typeName
    tell $ typeDefinition typeName
  pure $ "new " <> identify typeName <> "()"

typeDefinition :: Identifier -> Text
typeDefinition (Identifier typeName) = typeName <> " = function() {}\n"
