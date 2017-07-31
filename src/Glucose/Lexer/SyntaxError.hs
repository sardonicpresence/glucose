module Glucose.Lexer.SyntaxError where

import Data.Text
import Glucose.Source

type SyntaxError = Located SyntaxErrorDetails

data SyntaxErrorDetails = SyntaxError { message :: Text, context :: Text }
