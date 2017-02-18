module Glucose.Test.AST where

import Data.Text (Text)
import Glucose.AST
import Glucose.Identifier (Identifier(..))
import Glucose.Parser.Source
import Glucose.Test.Source

constant :: Text -> Literal -> FromSource Definition
constant name lit = definition name (Literal lit)

alias :: Text -> Text -> FromSource Definition
alias to from = definition to (Variable $ Identifier from)

definition :: Text -> Expression -> FromSource Definition
definition name value = fromSource $ Definition (identifier name) (fromSource value)

typeDefinition :: Text -> [Text] -> FromSource Definition
typeDefinition name ctors = fromSource $ TypeDefinition (identifier name) (map identifier ctors)
