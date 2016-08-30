module Glucose.TypeChecker where

import Control.Monad
import Glucose.AST
import Glucose.Error
import Glucose.Namespace

typeCheck :: Module -> Error Module
typeCheck m@(Module defs) = foldM_ (flip declare) emptyNamespace defs *> pure m
