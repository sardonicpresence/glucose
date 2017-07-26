module Glucose.Test.Source where

import Glucose.Source

fromSource :: a -> FromSource a
fromSource = FromSource (SourceRange beginning beginning)
