module Glucose.Test.IR.Unchecked (module Glucose.Test.IR.Core, alias) where

import Glucose.Test.IR.Core

import Control.Comonad
import Data.Text
import Glucose.IR

alias :: (Comonad f, Applicative f) => f Text -> f Text -> f (Definition Unchecked f)
alias to from = definition to $ reference UnknownKind from Unknown
