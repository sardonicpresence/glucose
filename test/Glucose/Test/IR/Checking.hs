module Glucose.Test.IR.Checking where

import Control.Lens
import Data.String
import Glucose.IR

box :: Type Checking -> Type Checking
box = dataType %~ boxed

unbox :: Type Checking -> Type Checking
unbox = dataType %~ unboxed

anyType :: Char -> Type Checking
anyType = Type . Any . toEnum . fromEnum

free :: Char -> Type Checking
free = Type . Free . toEnum . fromEnum

bound :: Char -> Type Checking
bound = Type . Bound . Polymorphic . fromString . pure

function :: Type Checking -> Type Checking -> Type Checking
function f a = Type . Bound $ Function UnknownArity f a
