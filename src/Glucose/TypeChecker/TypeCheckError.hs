module Glucose.TypeChecker.TypeCheckError where

import Glucose.Identifier
import Glucose.IR

data TypeCheckError f
  = DuplicateDefinition (f Identifier) (f ())
  | UnrecognisedVariable (f Identifier)
  | RecursiveDefinition (f Identifier)
  | TypeMismatch (f (Type Unchecked)) (f (Type Unchecked))

deriving instance (Eq (f Identifier), Eq (f ()), Eq (f (Type Unchecked))) => Eq (TypeCheckError f)
deriving instance (Show (f Identifier), Show (f ()), Show (f (Type Unchecked))) => Show (TypeCheckError f)
