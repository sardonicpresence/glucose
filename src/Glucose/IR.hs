{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
module Glucose.IR where

import Control.Comonad
import Data.List
import Glucose.Identifier
import Glucose.Parser.Source

class Annotations a where
  data Type a :: *
  data RefKind a :: *
  mkADT :: Identifier -> Type a
  intType :: Type a
  floatType :: Type a
  withType :: String -> Type a -> String
  withRefKind :: String -> RefKind a -> String

data Unchecked
instance Annotations Unchecked where
  data Type Unchecked = Unknown deriving (Eq)
  data RefKind Unchecked = UnknownKind deriving (Eq)
  mkADT = const Unknown
  intType = Unknown
  floatType = Unknown
  a `withType` _ = a
  a `withRefKind` _ = a

data Checked
instance Annotations Checked where
  data Type Checked = Integer | Float | ADT Identifier deriving (Eq)
  data RefKind Checked = Local | Global deriving (Eq)
  mkADT = ADT
  intType = Integer
  floatType = Float
  a `withType` ty = a ++ " : " ++ show ty
  a `withRefKind` Local = "%" ++ a
  a `withRefKind` Global = "@" ++ a

data Module ann = Module [FromSource (Definition ann)]
deriving instance (Eq (Type ann), Eq (RefKind ann)) => Eq (Module ann)

data Definition ann = Definition (FromSource Identifier) (FromSource (Expression ann))
deriving instance (Eq (Type ann), Eq (RefKind ann)) => Eq (Definition ann)

data Expression ann
  = Literal Literal
  | Reference (RefKind ann) Identifier (Type ann)
  | Constructor (FromSource Identifier) Int
deriving instance (Eq (Type ann), Eq (RefKind ann)) => Eq (Expression ann)

data Literal = IntegerLiteral Int | FloatLiteral Double deriving (Eq)

instance Show (Type Checked) where
  show Integer = "Int"
  show Float = "Float"
  show (ADT name) = show name

-- * Show instances

instance Annotations ann => Show (Module ann) where
  show (Module defs) = intercalate "\n\n" $ map (show . extract) defs

instance Annotations ann => Show (Definition ann) where
  show (Definition name value) =
    let n = show $ extract name
        declaration = n `withType` (typeOf (extract value) :: Type ann)
        definition = show name ++ " = " ++ show value
        -- definition = show (extract name) ++ " = " ++ show (extract value)
     in if declaration == n
          then definition
          else declaration ++ "\n" ++ definition

instance Annotations ann => Show (Expression ann) where
  show (Literal lit) = show lit `withType` (typeOf lit :: Type ann)
  show (Reference kind name ty) = (show name `withRefKind` kind) `withType` ty -- TODO: include arity
  show (Constructor typeName id) = show typeName ++ "#" ++ show id

instance Show Literal where
  show (IntegerLiteral a) = show a
  show (FloatLiteral a) = show a

-- * Types

class Annotations ann => Typed a ann where
  typeOf :: a -> Type ann

instance Annotations ann => Typed (Definition ann) ann where
  typeOf (Definition _ (FromSource _ e)) = typeOf e

instance Annotations ann => Typed (Expression ann) ann where
  typeOf (Literal a) = typeOf a
  typeOf (Reference _ _ ty) = ty
  typeOf (Constructor typeName _) = mkADT (extract typeName)

instance Annotations ann => Typed Literal ann where
  typeOf (IntegerLiteral _) = intType
  typeOf (FloatLiteral _) = floatType

instance Typed a ann => Typed (FromSource a) ann where
  typeOf (FromSource _ a) = typeOf a

-- * Bound instances

instance Bound (Definition ann) where
  identifier (Definition (FromSource _ name) _) = name
