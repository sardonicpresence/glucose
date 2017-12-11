module Glucose.TypeChecker.TypeCheckError where

import Glucose.Identifier
import Glucose.IR

data TypeCheckError f
  = DuplicateDefinition (f Identifier) (f ())
  | UnrecognisedVariable (f Identifier)
  | RecursiveDefinition (f Identifier)
  | TypeMismatch (Type Checked) (f (Type Checked))
  | InfiniteType (Type Checked) (f (Type Checked))
  | LocalLambda (f ())
  | CAF (f ())

deriving instance (Eq (f Identifier), Eq (f ()), Eq (f (Type Checked))) => Eq (TypeCheckError f)
deriving instance (Show (f Identifier), Show (f ()), Show (f (Type Checked)), Show (Type Checked)) => Show (TypeCheckError f)
