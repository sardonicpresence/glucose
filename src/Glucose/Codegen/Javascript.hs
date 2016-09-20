module Glucose.Codegen.JavaScript (codegen) where

import Control.Comonad
import Data.Monoid
import Data.Text (Text, pack, unpack)
import Glucose.Identifier
import Glucose.IR
import Glucose.Parser.Source

data JSRaw = JSRaw Text
instance Show JSRaw where
  show (JSRaw s) = unpack s

codegen :: Module -> JSRaw
codegen (Module defs) = JSRaw $ foldMap (definition . extract) defs

definition :: Definition -> Text
definition (Definition (FromSource _ (Identifier name)) def) = name <> " = " <> expression (extract def) <> "\n"

expression :: Expression -> Text
expression (Literal a) = pack $ show a
expression (Reference _ (Identifier a) _) = a
