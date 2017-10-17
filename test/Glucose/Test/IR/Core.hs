module Glucose.Test.IR.Core where

import Control.Comonad
import Data.Text (Text)
import Glucose.Identifier (Identifier (..))
import Glucose.IR

-- * Without source locations

constantAnywhere :: (Applicative f, Comonad f) => Text -> Literal -> f (Definition ann f)
constantAnywhere name lit = constant (pure name) (pure lit)

constructorAnywhere :: (Applicative f, Comonad f) => Text -> Text -> Int -> f (Definition ann f)
constructorAnywhere ty ctor = constructor (pure ty) (pure ctor)

definitionAnywhere :: (Applicative f, Comonad f) => Text -> Expression ann f -> f (Definition ann f)
definitionAnywhere name value = definition (pure name) (pure value)

-- * With source locations

constant :: (Applicative f, Comonad f) => f Text -> f Literal -> f (Definition ann f)
constant name lit = definition name (Literal <$> lit)

constructor :: Applicative f => f Text -> f Text -> Int -> f (Definition ann f)
constructor ty ctor index = ctor $> Constructor (Identifier <$> ctor) (Identifier <$> ty) index

definition :: (Applicative f, Comonad f) => f Text -> f (Expression ann f) -> f (Definition ann f)
definition name value = Definition <$> duplicate (Identifier <$> name) <*> duplicate value
