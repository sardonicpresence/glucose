module Glucose.TypeChecker.TypeCheckError where

import Glucose.Identifier
import Glucose.IR

data TypeCheckError f
  = DuplicateDefinition (f Identifier) (f ())
  | UnrecognisedVariable (f Identifier)
  | RecursiveDefinition (f Identifier)
  | TypeMismatch (f (Type Checking)) (f (Type Checking))
  | LocalLambda (f ())
  | CAF (f ())

deriving instance (Eq (f Identifier), Eq (f ()), Eq (f (Type Checking))) => Eq (TypeCheckError f)
deriving instance (Show (f Identifier), Show (f ()), Show (f (Type Checking))) => Show (TypeCheckError f)
