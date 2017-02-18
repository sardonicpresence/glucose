module Glucose.Test.Source where

import Data.Text
import Glucose.Identifier (Identifier(..))
import Glucose.Lexer.Location
import Glucose.Parser.Source

fromSource :: a -> FromSource a
fromSource = FromSource (SourceRange beginning beginning)

identifier :: Text -> FromSource Identifier
identifier = fromSource . Identifier
