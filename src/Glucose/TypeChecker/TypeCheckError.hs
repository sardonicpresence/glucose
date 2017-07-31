module Glucose.TypeChecker.TypeCheckError where

import Glucose.Identifier
import Glucose.IR.Checked

data TypeCheckError f
  = DuplicateDefinition (f Identifier) (f ())
  | UnrecognisedVariable (f Identifier)
  | RecursiveDefinition (f Identifier)
  | TypeMismatch (f Type) (f Type)
