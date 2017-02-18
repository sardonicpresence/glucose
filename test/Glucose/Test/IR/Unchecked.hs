module Glucose.Test.IR.Unchecked (module Glucose.Test.IR.Core, alias) where

import Glucose.Test.IR.Core

import Data.Text
import Glucose.IR
import Glucose.Parser.Source

alias :: FromSource Text -> FromSource Text -> FromSource (Definition Unchecked)
alias to from = definition to $ reference UnknownKind from Unknown
