module Glucose.Test.IR.Checking where

import Control.Lens
import Glucose.Identifier
import Glucose.IR

box :: Type Checking -> Type Checking
box = dataType %~ boxed

free :: Identifier -> Type Checking
free = Type . Free

bound :: Identifier -> Type Checking
bound = Type . Bound . Polymorphic

function :: Type Checking -> Type Checking -> Type Checking
function f a = Type . Bound $ Function UnknownArity f a
