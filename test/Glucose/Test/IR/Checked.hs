module Glucose.Test.IR.Checked (module Glucose.Test.IR.Core, module Glucose.Test.IR.Checked) where

import Glucose.Test.IR.Core

import Control.Comonad
import Data.Text
import Glucose.Identifier
import Glucose.IR

alias :: (Comonad f, Applicative f) => f Text -> f Text -> DataType (Type Checked) -> f (Definition Checked f)
alias to from ty = definition to $ reference (Global <$> from) ty

alias' :: (Comonad f, Applicative f) => Text -> Text -> DataType (Type Checked) -> f (Definition Checked f)
alias' to from ty = definition' to $ Reference (Global $ Identifier from) (Type $ Checked ty)

function :: (Comonad f, Applicative f) => f Text -> f a -> f Text -> DataType (Type Checked) -> f (Expression Checked f) -> f (Definition Checked f)
function name loc arg ty def = definition name $
  Lambda <$ loc <*> duplicate (flip argument ty <$> arg) <*> duplicate def

function' :: (Comonad f, Applicative f) => Text -> Text -> DataType (Type Checked) -> f (Expression Checked f) -> f (Definition Checked f)
function' name arg = function (pure name) (pure ()) (pure arg)

apply :: (Comonad f, Applicative f) => f (Expression Checked f) -> f (Expression Checked f) -> DataType (Type Checked) -> f (Expression Checked f)
apply f a ty = (\f a -> Apply f a $ Type $ Checked ty) <$> duplicate f <*> duplicate a

apply' :: (Applicative f, Comonad f)
 => (DataType (Type Checked) -> f (Expression Checked f)) -> (DataType (Type Checked) -> f (Expression Checked f))
 -> DataType (Type Checked) -> DataType (Type Checked) -> f (Expression Checked f)
apply' f a from to = apply (f $ from --> to) (a from) to

reference :: Functor f => f (Ref Checked Text) -> DataType (Type Checked) -> f (Expression Checked f)
reference ref ty = (\n -> Reference (Identifier <$> n) (Type $ Checked ty)) <$> ref

reference' :: Applicative f => Ref Checked Text -> DataType (Type Checked) -> f (Expression Checked f)
reference' = reference . pure

local' :: Applicative f => Text -> DataType (Type Checked) -> f (Expression Checked f)
local' a = reference' $ Local a

global' :: Applicative f => Text -> DataType (Type Checked) -> f (Expression Checked f)
global' a = reference' $ Global a

argument :: Text -> DataType (Type Checked) -> Arg Checked
argument name = Arg (Identifier name) . Type . Checked

infixr 9 -->

(-->) :: DataType (Type Checked) -> DataType (Type Checked) -> DataType (Type Checked)
from --> to = Function (Arity 1) (Type $ Checked from) (Type $ Checked to)
