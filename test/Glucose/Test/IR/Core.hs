module Glucose.Test.IR.Core where

import Control.Comonad
import Data.Text (Text)
import Glucose.Identifier (Identifier (..))
import Glucose.IR

-- * Without source locations

constant' :: (Applicative f, Comonad f) => Text -> Literal -> f (Definition ann f)
constant' name lit = constant (pure name) (pure lit)

constructor' :: (Applicative f, Comonad f) => Text -> Text -> Int -> f (Definition ann f)
constructor' ty ctor = constructor (pure ty) (pure ctor)

definition' :: (Applicative f, Comonad f) => Text -> Expression ann f -> f (Definition ann f)
definition' name value = definition (pure name) (pure value)

literal' :: Applicative f => Literal -> f (Expression ann f)
literal' = literal . pure

integer' :: Applicative f => Int -> f (Expression ann f)
integer' = literal . pure . IntegerLiteral

float' :: Applicative f => Double -> f (Expression ann f)
float' = literal . pure . FloatLiteral

-- * With source locations

constant :: (Applicative f, Comonad f) => f Text -> f Literal -> f (Definition ann f)
constant name lit = definition name (Literal <$> lit)

constructor :: Applicative f => f Text -> f Text -> Int -> f (Definition ann f)
constructor ty ctor index = ctor $> Constructor (Identifier <$> ctor) (Identifier <$> ty) index

definition :: (Applicative f, Comonad f) => f Text -> f (Expression ann f) -> f (Definition ann f)
definition name value = Definition <$> duplicate (Identifier <$> name) <*> duplicate value

literal :: Functor f => f Literal -> f (Expression ann f)
literal value = Literal <$> value
