module Glucose.Test.IR where

import Data.Text
import Glucose.Identifier
import Glucose.IR
import Glucose.Parser.Source
import Glucose.Test.Source

constant :: Text -> Literal -> FromSource Definition
constant name lit = definition name (Literal lit)

alias :: Text -> Text -> Type -> FromSource Definition
alias to from ty = definition to $ Reference Global (Identifier from) ty

definition :: Text -> Expression -> FromSource Definition
definition name value = fromSource $ Definition (fromSource $ Identifier name) (fromSource value)
