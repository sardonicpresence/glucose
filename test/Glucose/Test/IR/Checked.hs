module Glucose.Test.IR.Checked (module Glucose.Test.IR.Core, alias, aliasAnywhere) where

import Glucose.Test.IR.Core

import Data.Text
import Glucose.Identifier
import Glucose.IR
import Glucose.Source

alias :: FromSource Text -> FromSource Text -> Type Checked -> FromSource (Definition Checked)
alias to from ty = definition to $ reference Global from ty

aliasAnywhere :: Text -> Text -> Type Checked -> FromSource (Definition Checked)
aliasAnywhere to from ty = definitionAnywhere to $ Reference Global (Identifier from) ty ty
