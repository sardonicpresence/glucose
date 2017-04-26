{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
module Glucose.IR where

import Control.Comonad
import Data.List
import Glucose.Identifier
import Glucose.Parser.Source

newtype Module ann = Module [FromSource (Definition ann)]
deriving instance (Eq (Type ann), Eq (RefKind ann)) => Eq (Module ann)

data Definition ann = Definition (FromSource Identifier) (FromSource (Expression ann))
deriving instance (Eq (Type ann), Eq (RefKind ann)) => Eq (Definition ann)

data Expression ann
  = Literal Literal
  | Reference (RefKind ann) Identifier (Type ann) (Type ann)
  | Constructor (FromSource Identifier) Int -- TODO: does not belong
  | Lambda [FromSource (Arg ann)] (FromSource (Expression ann))
  | Apply (FromSource (Expression ann)) (FromSource (Expression ann))
deriving instance (Eq (Type ann), Eq (RefKind ann)) => Eq (Expression ann)

data Literal = IntegerLiteral Int | FloatLiteral Double deriving (Eq)

data Arg ann = Arg Identifier (Type ann)
deriving instance Eq (Type ann) => Eq (Arg ann)
deriving instance Ord (Type ann) => Ord (Arg ann)

instance Show (Type Checked) where
  show Integer = "Int"
  show Float = "Float"
  show (Boxed ty) = "{" ++ show ty ++ "}"
  show (ADT name) = show name
  show (Function ar arg@Function{} ret) = "(" ++ show arg ++ ")" ++ show ar ++ show ret
  show (Function ar arg ret) = show arg ++ show ar ++ show ret
  show (Bound name) = show name
  show (Free name) = "*" ++ show name

argTypes :: Type Checked -> [Type Checked]
argTypes (Function _ a b) = a : argTypes b
argTypes _ = []

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
  show (Reference kind name rep ty) = (show name `withRefKind` kind) `withType` ty `withType` rep -- TODO: include arity
  show (Constructor typeName id) = show typeName ++ "#" ++ show id
  show (Lambda args value) = "\\" ++ unwords (map (show.extract) args) ++ " -> " ++ show (extract value)
  show (Apply expr arg) = show (extract expr) ++ " (" ++ show (extract arg) ++ ")"

instance Annotations ann => Show (Arg ann) where
  show (Arg name ty) = show name `withType` ty

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
  typeOf (Reference _ _ _ ty) = ty
  typeOf (Constructor typeName _) = mkADT (extract typeName)
  typeOf (Lambda args expr) = go 0 args where
    go _ [] = typeOf $ extract expr
    go m (a:as) = funType (Arity n m) (typeOf $ extract a) $ go (m+1) as
    n = length args
  typeOf (Apply f _) = returnType (typeOf f)

instance Annotations ann => Typed Literal ann where
  typeOf (IntegerLiteral _) = intType
  typeOf (FloatLiteral _) = floatType

instance Annotations ann => Typed (Arg ann) ann where
  typeOf (Arg _ ty) = ty

instance Typed a ann => Typed (FromSource a) ann where
  typeOf (FromSource _ a) = typeOf a

-- * Bound instances

instance Bound (Definition ann) where
  identifier (Definition (extract -> name) _) = name

instance Bound (Arg ann) where
  identifier (Arg name _) = name

-- * Annotations

class Annotations a where
  data Type a :: *
  data RefKind a :: *
  mkADT :: Identifier -> Type a
  intType :: Type a
  floatType :: Type a
  funType :: Arity -> Type a -> Type a -> Type a
  returnType :: Type a -> Type a
  withType :: String -> Type a -> String
  withRefKind :: String -> RefKind a -> String

data Unchecked
instance Annotations Unchecked where
  data Type Unchecked = Unknown deriving (Eq)
  data RefKind Unchecked = UnknownKind deriving (Eq)
  mkADT = const Unknown
  intType = Unknown
  floatType = Unknown
  funType _ _ _ = Unknown
  returnType _ = Unknown
  a `withType` _ = a
  a `withRefKind` _ = a

data Checked
instance Annotations Checked where
  data Type Checked = Integer | Float | ADT Identifier | Function Arity (Type Checked) (Type Checked)
                    | Bound Identifier | Boxed (Type Checked)
                    | Free Identifier -- Must be eliminated by type-checking (should be made seperate)
                    deriving (Eq, Ord)
  data RefKind Checked = Local | Global deriving (Eq)
  mkADT = ADT
  intType = Integer
  floatType = Float
  funType = Function
  returnType (Function _ _ b) = b
  returnType _ = error "Cannot pass argument to a non-function!"
  a `withType` ty = a ++ ":" ++ show ty
  a `withRefKind` Local = "%" ++ a
  a `withRefKind` Global = "@" ++ a

data Arity = UnknownArity | Arity Int Int deriving (Eq, Ord)

instance Show Arity where
  show _ = "->"
  -- show UnknownArity = "-?>"
  -- show (Arity n 0) = "-" ++ show n ++ ">"
  -- show (Arity n m) = "-" ++ show n ++ "/" ++ show m ++ ">"
