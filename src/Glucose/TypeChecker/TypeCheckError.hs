module Glucose.TypeChecker.TypeCheckError where

import Glucose.Identifier
import Glucose.IR.Checked

data TypeCheckError f
  = DuplicateDefinition (f Identifier) (f ())
  | UnrecognisedVariable (f Identifier)
  | RecursiveDefinition (f Identifier)
  | TypeMismatch (f Type) (f Type)

deriving instance (Eq (f Identifier), Eq (f ()), Eq (f Type)) => Eq (TypeCheckError f)
deriving instance (Show (f Identifier), Show (f ()), Show (f Type)) => Show (TypeCheckError f)
