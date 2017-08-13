{-# LANGUAGE TypeFamilies, FunctionalDependencies #-}
module Glucose.IR
(
  Module(..), Definition(..), Expression(..), Literal(..), Arg(..),
  Annotations(..), ReferenceAnnotation(..), Untyped, Unchecked, Checked, Type(..),
  Primitive(..), DataType(..), Arity(..), ReferenceKind(..),
  Typed(..), types, replaceType, checkedType, uncheckedType
)
where

import Control.Comonad
import Control.Comonad.Utils
import Control.Lens
import Data.List
import Glucose.Identifier

newtype Module ann f = Module [f (Definition ann f)]
deriving instance (Eq (f (Definition ann f))) => Eq (Module ann f)

data Definition ann f = Definition (f Identifier) (f (Expression ann f))
                      | Constructor (f Identifier) (f Identifier) Int
deriving instance (Eq (f Identifier), Eq (f (Expression ann f))) => Eq (Definition ann f)

data Expression ann f
  = Literal Literal
  | Reference (Ref ann) Identifier (Type ann) (Type ann)
  | Lambda [f (Arg ann)] (f (Expression ann f))
  | Apply (f (Expression ann f)) (f (Expression ann f)) (Type ann)
deriving instance (Eq (Type ann), Eq (Ref ann), Eq (f Identifier), Eq (f (Arg ann)), Eq (f (Expression ann f))) => Eq (Expression ann f)

data Literal = IntegerLiteral Int | FloatLiteral Double deriving (Eq)

data Arg ann = Arg Identifier (Type ann)
deriving instance Eq (Type ann) => Eq (Arg ann)
deriving instance Ord (Type ann) => Ord (Arg ann)


-- * Annotations

class ReferenceAnnotation (Ref ann) => Annotations ann where
  data Type ann :: *
  type Ref ann :: *
  withType :: String -> Type ann -> String
  dataType :: DataType (Type ann) -> Type ann

data Untyped
data Unchecked
data Checked


-- * References

class ReferenceAnnotation a where
  showRef :: Show b => a -> b -> String

instance ReferenceAnnotation () where
  showRef _ = show

data ReferenceKind = Local | Global deriving (Eq)

instance ReferenceAnnotation ReferenceKind where
  showRef Local a = "%" ++ show a
  showRef Global a = "@" ++ show a


-- * Types

data Primitive = Integer | Float
  deriving (Eq, Ord)

data Arity = UnknownArity | Arity Int deriving (Eq, Ord)

data DataType t = Unboxed Primitive | Boxed Primitive | ADT Identifier | Function Arity t t | Polymorphic Identifier
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Annotations Untyped where
  data Type Untyped = Untyped
  type Ref Untyped = ()
  withType = const
  dataType = const Untyped

instance Annotations Unchecked where
  data Type Unchecked = Free Identifier | Bound (DataType (Type Unchecked))
  type Ref Unchecked = ReferenceKind
  name `withType` ty = name ++ ":" ++ show ty
  dataType = Bound

instance Annotations Checked where
  newtype Type Checked = Known (DataType (Type Checked)) deriving (Eq, Ord)
  type Ref Checked = ReferenceKind
  name `withType` ty = name ++ ":" ++ show ty
  dataType = Known

instance Show (Type Unchecked) where
  show (Free name) = "*" ++ show name
  show (Bound ty) = show ty

instance Show (Type Checked) where
  show (Known ty) = show ty

instance Show Arity where
  show _ = "->"
  -- show UnknownArity = "-?>"
  -- show (Arity n 0) = "-" ++ show n ++ ">"
  -- show (Arity n m) = "-" ++ show n ++ "/" ++ show m ++ ">"

-- _Free :: Prism' (Type Unchecked) Identifier
-- _Free = prism' Free $ \case Free a -> Just a; _ -> Nothing
--
-- _Bound :: Prism' (Type Unchecked) Identifier
-- _Bound = prism' Bound $ \case Bound a -> Just a; _ -> Nothing
--
-- _Known :: Prism' (Type Unchecked) (ConcreteType (Type Unchecked))
-- _Known = prism' Known $ \case Known a -> Just a; _ -> Nothing

replaceType :: (Eq (Type ann), Traversable f) => Type ann -> Type ann -> Expression ann f -> Expression ann f
replaceType from to = types . filtered (== from) .~ {- boxed -} to

checkedType :: Applicative f => f Identifier -> Type Unchecked -> f (Type Checked)
checkedType newName ty = Known <$> case ty of
  Free _ -> Polymorphic <$> newName
  Bound ty -> traverse (checkedType newName) ty

uncheckedType :: Type Checked -> Type Unchecked
uncheckedType (Known ty) = Bound $ uncheckedType <$> ty


-- * Show instances

instance (Comonad f, Annotations ann) => Show (Module ann f) where
  show (Module defs) = intercalate "\n\n" $ map (show . extract) defs

instance (Comonad f, Annotations ann) => Show (Definition ann f) where
  show (Definition n value) = if declaration == name then definition else declaration ++ "\n" ++ definition where
    name = show $ extract n
    declaration = name `withType` typeOf (extract value)
    definition = name ++ " = " ++ show (extract value)
  show (Constructor name typeName index) = show (extract name) ++ " = " ++ show (extract typeName) ++ "#" ++ show index

instance (Comonad f, Annotations ann) => Show (Expression ann f) where
  show (Literal lit) = show lit
  show (Reference kind name rep ty) = showRef kind name `withType` ty `withType` rep -- TODO: include arity
  show (Lambda args value) = "\\" ++ unwords (map (show . extract) args) ++ " -> " ++ show (extract value)
  show (Apply expr arg ty) = "((" ++ show (extract expr) ++ ") (" ++ show (extract arg) ++ "))" `withType` ty

instance Annotations ann => Show (Arg ann) where
  show (Arg name ty) = show name `withType` ty

instance Show Literal where
  show (IntegerLiteral a) = show a
  show (FloatLiteral a) = show a

instance Show t => Show (DataType t) where
  show (Unboxed ty) = show ty
  show (Boxed ty) = "{" ++ show ty ++ "}"
  show (ADT name) = show name
  -- show (Function ar arg@Function{} ret) = "(" ++ show arg ++ ")" ++ show ar ++ show ret
  show (Function ar arg ret) = "(" ++ show arg ++ show ar ++ show ret ++ ")"
  show (Polymorphic name) = "*" ++ show name

instance Show Primitive where
  show Integer = "Int"
  show Float = "Float"


-- * Typed

class Typed ann a | a -> ann where
  typeOf :: a -> Type ann

instance (Comonad f, Annotations ann) => Typed ann (Definition ann f) where
  typeOf (Definition _ e) = typeOf $ extract e
  typeOf (Constructor _ typeName _) = dataType . ADT $ extract typeName

instance (Comonad f, Annotations ann) => Typed ann (Expression ann f) where
  typeOf (Literal (IntegerLiteral _)) = dataType $ Unboxed Integer
  typeOf (Literal (FloatLiteral _)) = dataType $ Unboxed Float
  typeOf (Reference _ _ _ ty) = ty
  typeOf (Lambda args expr) = go (length args) args where
    go _ [] = typeOf $ extract expr
    go m (a:as) = dataType $ Function (Arity m) (typeOf $ extract a) (go (m-1) as)
  typeOf (Apply _ _ ty) = ty

instance Typed ann (Arg ann) where
  typeOf (Arg _ ty) = ty

types :: (Traversable f, Ref from ~ Ref to) => Traversal (Expression from f) (Expression to f) (Type from) (Type to)
types _ (Literal lit) = pure $ Literal lit
types f (Reference kind name rep ty) = Reference kind name <$> f rep <*> f ty
types f (Lambda args expr) = Lambda <$> traverse (traverse $ argType f) args <*> traverse (types f) expr where
  argType f (Arg name ty) = Arg name <$> f ty
types f (Apply fun arg ty) = Apply <$> traverse (types f) fun <*> traverse (types f) arg <*> f ty


-- * Bound instances

instance Bound f (Definition ann f) where
  identifier (Definition name _) = name
  identifier (Constructor name _ _) = name

instance Comonad f => Bound f (f (Definition ann f)) where
  identifier = identifier . extract

instance Functor f => Bound f (f (Arg ann)) where
  identifier = fmap $ \case Arg name _ -> name

-- argTypes :: Type Checked -> [Type Checked]
-- argTypes (Function _ a b) = a : argTypes b
-- argTypes _ = []
