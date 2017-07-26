module Glucose.Test.Source where

import Glucose.Source

anywhere :: a -> FromSource a
anywhere = FromSource (SourceRange undefined undefined)
