module Glucose.Test.IR.Checked (module Glucose.Test.IR.Core, alias, aliasAnywhere, function, reference) where

import Glucose.Test.IR.Core

import Control.Comonad
import Data.Text
import Glucose.Identifier
import Glucose.IR

alias :: (Comonad f, Applicative f) => f Text -> f Text -> DataType (Type Checked) -> f (Definition Checked f)
alias to from ty = definition to $ reference Global from ty

aliasAnywhere :: (Comonad f, Applicative f) => Text -> Text -> DataType (Type Checked) -> f (Definition Checked f)
aliasAnywhere to from ty = definitionAnywhere to $ Reference Global (Identifier from) (Type $ Checked ty)

function :: (Applicative f, Comonad f) => f Text -> f a -> f Text -> DataType (Type Checked) -> f (Expression Checked f) -> f (Definition Checked f)
function name loc arg ty def = definition name $
  Lambda <$ loc <*> duplicate (flip argument ty <$> arg) <*> duplicate def

reference :: Functor f => Ref Checked -> f Text -> DataType (Type Checked) -> f (Expression Checked f)
reference kind name ty = (\n -> Reference kind (Identifier n) (Type $ Checked ty)) <$> name

argument :: Text -> DataType (Type Checked) -> Arg Checked
argument name = Arg (Identifier name) . Type . Checked
