module Glucose.Codegen.JavaScript (codegen) where

import Control.Comonad
import Control.Monad.RWS
import Data.Set as Set
import Data.Text (Text, pack, unpack)
import Glucose.Identifier
import Glucose.IR
import Glucose.Parser.Source

data JSRaw = JSRaw Text
instance Show JSRaw where
  show (JSRaw s) = unpack s

type Codegen = RWS () Text (Set Identifier)

codegen :: Module Checked -> JSRaw
codegen (Module defs) = JSRaw . snd $ evalRWS (mapM_ (tell <=< definition . extract) defs) () empty

definition :: Definition Checked -> Codegen Text
definition (Definition (FromSource _ (Identifier name)) def) = do
  expr <- expression (extract def)
  pure $ name <> " = " <> expr <> "\n"

expression :: Expression Checked -> Codegen Text
expression (Literal a) = pure . pack $ show a
expression (Reference _ (Identifier a) _) = pure a
expression (Constructor (extract -> typeName) _) = do
  typeDefined <- Set.member typeName <$> get
  unless typeDefined $ do
    modify $ Set.insert typeName
    tell $ typeDefinition typeName
  pure $ "new " <> identify typeName <> "()"

typeDefinition :: Identifier -> Text
typeDefinition (Identifier typeName) = typeName <> " = function() {}\n"
