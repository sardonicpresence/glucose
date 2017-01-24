module Glucose.Test.IR where

import Data.Text
import Glucose.Identifier
import Glucose.IR
import Glucose.Parser.Source
import Glucose.Test.Source

constant :: Text -> Literal -> Definition
constant name lit = definition name (Literal lit)

alias :: Text -> Text -> Type -> Definition
alias to from ty = definition to $ Reference Global (Identifier from) ty

definition :: Text -> Expression -> Definition
definition name value = Definition (fromSource $ Identifier name) (fromSource value)
