module Glucose.IR where

import Control.Comonad
import Data.List
import Glucose.Identifier
import Glucose.Parser.Source

data Module = Module [FromSource Definition] deriving (Eq)

data Definition = Definition (FromSource Identifier) (FromSource Expression) deriving (Eq)

data Expression
  = Literal Literal
  | Reference ReferenceKind Identifier Type
  deriving (Eq)

data ReferenceKind = Local | Global deriving (Eq)

data Literal = IntegerLiteral Int | FloatLiteral Double deriving (Eq)

data Type = Integer | Float
  deriving (Eq)

instance Show Type where
  show Integer = "Int"
  show Float = "Float"

-- * Show instances

instance Show Module where
  show (Module defs) = intercalate "\n\n" $ map (show . extract) defs

instance Show Definition where
  show (Definition name value) =
    show (extract name) ++ " : " ++ show (typeOf $ extract value) ++ "\n" ++
    show (extract name) ++ " = " ++ show (extract value)

instance Show Expression where
  show (Literal lit) = show lit ++ ":" ++ show (typeOf lit)
  show (Reference kind name ty) = show kind ++ show name ++ ":" ++ show ty -- TODO: include arity

instance Show ReferenceKind where
  show Local = "%"
  show Global = "@"

instance Show Literal where
  show (IntegerLiteral a) = show a
  show (FloatLiteral a) = show a

-- * Types

class Typed a where
  typeOf :: a -> Type

instance Typed Definition where
  typeOf (Definition _ (FromSource _ e)) = typeOf e

instance Typed Expression where
  typeOf (Literal a) = typeOf a
  typeOf (Reference _ _ ty) = ty

instance Typed Literal where
  typeOf (IntegerLiteral _) = Integer
  typeOf (FloatLiteral _) = Float

instance Typed a => Typed (FromSource a) where
  typeOf (FromSource _ a) = typeOf a

-- * Bound instances

instance Bound Definition where
  identifier (Definition (FromSource _ name) _) = name
