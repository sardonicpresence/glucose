module Glucose.Test.IR.Checked (module Glucose.Test.IR.Core, module Glucose.Test.IR.Checked) where

import Glucose.Test.IR.Core

import Control.Comonad
import Data.Text
import Glucose.Identifier
import Glucose.IR

alias :: (Comonad f, Applicative f) => f Text -> f Text -> DataType (Type Checked) -> f (Definition Checked f)
alias to from ty = definition to $ reference Global from ty

aliasAnywhere :: (Comonad f, Applicative f) => Text -> Text -> DataType (Type Checked) -> f (Definition Checked f)
aliasAnywhere to from ty = definitionAnywhere to $ Reference Global (Identifier from) (Type $ Checked ty)

function :: (Comonad f, Applicative f) => f Text -> f a -> f Text -> DataType (Type Checked) -> f (Expression Checked f) -> f (Definition Checked f)
function name loc arg ty def = definition name $
  Lambda <$ loc <*> duplicate (flip argument ty <$> arg) <*> duplicate def

functionAnywhere :: (Comonad f, Applicative f) => Text -> Text -> DataType (Type Checked) -> f (Expression Checked f) -> f (Definition Checked f)
functionAnywhere name arg = function (pure name) (pure ()) (pure arg)

apply :: (Comonad f, Applicative f) => f (Expression Checked f) -> f (Expression Checked f) -> DataType (Type Checked) -> f (Expression Checked f)
apply f a ty = (\f a -> Apply f a $ Type $ Checked ty) <$> duplicate f <*> duplicate a

reference :: Functor f => Ref Checked -> f Text -> DataType (Type Checked) -> f (Expression Checked f)
reference kind name ty = (\n -> Reference kind (Identifier n) (Type $ Checked ty)) <$> name

referenceAnywhere :: Applicative f => Ref Checked -> Text -> DataType (Type Checked) -> f (Expression Checked f)
referenceAnywhere kind name = reference kind (pure name)

argument :: Text -> DataType (Type Checked) -> Arg Checked
argument name = Arg (Identifier name) . Type . Checked

functionType :: DataType (Type Checked) -> DataType (Type Checked) -> DataType (Type Checked)
functionType from to = Function UnknownArity (Type $ Checked from) (Type $ Checked to)
