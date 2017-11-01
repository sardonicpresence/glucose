module Glucose.Test.IR.Unchecked (module Glucose.Test.IR.Core, alias, function, apply, apply', reference) where

import Glucose.Test.IR.Core

import Control.Comonad
import Data.Text
import Glucose.Identifier (Identifier (..))
import Glucose.IR

alias :: (Comonad f, Applicative f) => f Text -> f Text -> f (Definition Unchecked f)
alias to from = definition to $ reference from

function :: (Applicative f, Comonad f) => f Text -> f a -> f Text -> f (Expression Unchecked f) -> f (Definition Unchecked f)
function name loc arg def = definition name $
  Lambda <$ loc <*> duplicate (argument <$> arg) <*> duplicate def

apply :: (Comonad f, Applicative f) => f (Expression Unchecked f) -> f (Expression Unchecked f) -> f (Expression Unchecked f)
apply f a = (\f a -> Apply f a $ Type Untyped) <$> duplicate f <*> duplicate a

apply' :: (Comonad f, Applicative f) => f Text -> f Text -> f (Expression Unchecked f)
apply' f a = (\f a -> Apply f a $ Type Untyped) <$> duplicate (reference f) <*> duplicate (reference a)

reference :: Functor f => f Text -> f (Expression Unchecked f)
reference name = (\n -> Reference (pure $ Identifier n) (Type Untyped)) <$> name

argument :: Text -> Arg Unchecked
argument name = Arg (Identifier name) (Type Untyped)
