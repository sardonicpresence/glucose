module Glucose.Lexer.SyntaxError where

import Data.Text
import Glucose.Source

data SyntaxError = SyntaxError Location SyntaxErrorDetails deriving (Eq, Show)

data SyntaxErrorDetails = SyntaxErrorDetails { message :: Text, context :: Text } deriving (Eq, Show)

instance Located SyntaxError where
  location (SyntaxError loc _) = loc
