module Glucose.Error (module Control.Monad.Except, SyntaxError, Error) where

import Control.Monad.Except

type SyntaxError = String

type Error = Either SyntaxError
