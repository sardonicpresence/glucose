{-# LANGUAGE ScopedTypeVariables, TypeFamilies #-}
module Glucose.IR where

import Control.Comonad
import Data.List
import Glucose.Identifier

newtype Module ann f = Module [f (Definition ann f)]
deriving instance (Eq (f (Definition ann f))) => Eq (Module ann f)

data Definition ann f = Definition (f Identifier) (f (Expression ann f))
                      | Constructor (f Identifier) (f Identifier) Int
deriving instance (Eq (f Identifier), Eq (f (Expression ann f))) => Eq (Definition ann f)

data Expression ann f
  = Literal Literal
  | Reference (RefKind ann) Identifier (Type ann) (Type ann)
  | Lambda [f (Arg ann)] (f (Expression ann f))
  | Apply (f (Expression ann f)) (f (Expression ann f))
deriving instance (Eq (Type ann), Eq (RefKind ann), Eq (f Identifier), Eq (f (Arg ann)), Eq (f (Expression ann f))) => Eq (Expression ann f)

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

instance (Comonad f, Annotations ann) => Show (Module ann f) where
  show (Module defs) = intercalate "\n\n" $ map (show . extract) defs

instance (Comonad f, Annotations ann) => Show (Definition ann f) where
  show (Definition name value) =
    let n = show $ extract name
        declaration = n `withType` (typeOf (extract value) :: Type ann)
        definition = show (extract name) ++ " = " ++ show (extract value)
     in if declaration == n
          then definition
          else declaration ++ "\n" ++ definition
  show (Constructor name typeName id) = show (extract name) ++ " = " ++ show (extract typeName) ++ "#" ++ show id

instance (Comonad f, Annotations ann) => Show (Expression ann f) where
  show (Literal lit) = show lit `withType` (typeOf lit :: Type ann)
  show (Reference kind name rep ty) = (show name `withRefKind` kind) `withType` ty `withType` rep -- TODO: include arity
  show (Lambda args value) = "\\" ++ unwords (map (show . extract) args) ++ " -> " ++ show (extract value)
  show (Apply expr arg) = show (extract expr) ++ " (" ++ show (extract arg) ++ ")"

instance Annotations ann => Show (Arg ann) where
  show (Arg name ty) = show name `withType` ty

instance Show Literal where
  show (IntegerLiteral a) = show a
  show (FloatLiteral a) = show a

-- * Types

class Annotations ann => Typed a ann where
  typeOf :: a -> Type ann

instance (Comonad f, Annotations ann) => Typed (Definition ann f) ann where
  typeOf (Definition _ e) = typeOf $ extract e
  typeOf (Constructor _ typeName _) = mkADT (extract typeName)

instance (Comonad f, Annotations ann) => Typed (Expression ann f) ann where
  typeOf (Literal a) = typeOf a
  typeOf (Reference _ _ _ ty) = ty
  typeOf (Lambda args expr) = go n args where
    go _ [] = typeOf $ extract expr
    go m (a:as) = funType (Arity m) (typeOf $ extract a) $ go (m-1) as
    n = length args
  typeOf (Apply f _) = returnType (typeOf $ extract f)

instance Annotations ann => Typed Literal ann where
  typeOf (IntegerLiteral _) = intType
  typeOf (FloatLiteral _) = floatType

instance Annotations ann => Typed (Arg ann) ann where
  typeOf (Arg _ ty) = ty

-- * Bound instances

instance Bound f (Definition ann f) where
  identifier (Definition name _) = name
  identifier (Constructor name _ _) = name

instance Comonad f => Bound f (f (Definition ann f)) where
  identifier = identifier . extract

instance Functor f => Bound f (f (Arg ann)) where
  identifier = fmap $ \case Arg name _ -> name

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

data Arity = UnknownArity | Arity Int deriving (Eq, Ord)

instance Show Arity where
  show _ = "->"
  -- show UnknownArity = "-?>"
  -- show (Arity n 0) = "-" ++ show n ++ ">"
  -- show (Arity n m) = "-" ++ show n ++ "/" ++ show m ++ ">"
