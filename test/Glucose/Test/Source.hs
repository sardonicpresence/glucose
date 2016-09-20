module Glucose.Test.Source where

import Glucose.Lexer.Location
import Glucose.Parser.Source

fromSource :: a -> FromSource a
fromSource = FromSource (SourceRange beginning beginning)
