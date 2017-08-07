module Glucose.Test.IR.Checked (module Glucose.Test.IR.Core, alias, aliasAnywhere) where

import Glucose.Test.IR.Core

import Control.Comonad
import Data.Text
import Glucose.Identifier
import Glucose.IR

alias :: (Comonad f, Applicative f) => f Text -> f Text -> Type Checked -> f (Definition Checked f)
alias to from ty = definition to $ reference Global from ty

aliasAnywhere :: (Comonad f, Applicative f) => Text -> Text -> Type Checked -> f (Definition Checked f)
aliasAnywhere to from ty = definitionAnywhere to $ Reference Global (Identifier from) ty ty
