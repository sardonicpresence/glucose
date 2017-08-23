{-# LANGUAGE TypeFamilies, FunctionalDependencies #-}
module Glucose.IR
(
  Module(..), Definition(..), Expression(..), Literal(..), Arg(..),
  Annotations(..), ReferenceAnnotation(..), Unchecked, Checking, Checked, Type(..), TypeF(..),
  Primitive(..), DataType(..), Arity(..), ReferenceKind(..),
  dataType, typeVariables, free, uncheck, bind, types, bindings, boxed,
  Typed(..), typeAnnotations, replaceType
)
where

import Control.Comonad
import Control.Lens
import Data.List
import Glucose.Identifier
import Unsafe.Coerce

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

class (Traversable (TypeF ann), ReferenceAnnotation (Ref ann)) => Annotations ann where
  data TypeF ann :: * -> *
  type Ref ann :: *
  withType :: String -> Type ann -> String
  typeF :: Prism' (TypeF ann a) a

newtype Type ann = Type (TypeF ann (DataType (Type ann)))

deriving instance Eq (TypeF ann (DataType (Type ann))) => Eq (Type ann)
deriving instance Ord (TypeF ann (DataType (Type ann))) => Ord (Type ann)

instance Show (TypeF ann (DataType (Type ann))) => Show (Type ann) where
  show (Type ty) = show ty

_Type :: Iso (Type from) (Type to) (TypeF from (DataType (Type from))) (TypeF to (DataType (Type to)))
_Type = flip iso Type $ \(Type ty) -> ty

dataType :: Annotations ann => Prism' (Type ann) (DataType (Type ann))
dataType = _Type . typeF

data Unchecked
data Checking
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

data Arity = UnknownArity | Arity Int
  deriving (Eq, Ord)

data DataType t = Unboxed Primitive | Boxed Primitive | ADT Identifier | Function Arity t t | Polymorphic Identifier
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Annotations Unchecked where
  data TypeF Unchecked ty = Untyped
    deriving (Eq, Functor, Foldable, Traversable)
  type Ref Unchecked = ()
  withType = const
  typeF = prism (const Untyped) Left

instance Annotations Checking where
  data TypeF Checking ty = Free Identifier | Bound ty
    deriving (Eq, Functor, Foldable, Traversable)
  type Ref Checking = ReferenceKind
  name `withType` ty = name ++ ":" ++ show ty
  typeF = prism' Bound $ \case Bound ty -> Just ty; _ -> Nothing

instance Annotations Checked where
  newtype TypeF Checked ty = Checked ty
    deriving (Eq, Ord, Functor, Foldable, Traversable)
  type Ref Checked = ReferenceKind
  name `withType` ty = name ++ ":" ++ show ty
  typeF = prism' Checked $ \(Checked ty) -> Just ty

instance Show ty => Show (TypeF Checking ty) where
  show (Free name) = "*" ++ show name
  show (Bound ty) = show ty

instance Show ty => Show (TypeF Checked ty) where
  show (Checked ty) = show ty

instance Show Arity where
  show _ = "->"
  -- show UnknownArity = "-?>"
  -- show (Arity n 0) = "-" ++ show n ++ ">"
  -- show (Arity n m) = "-" ++ show n ++ "/" ++ show m ++ ">"

typeVariables :: (Annotations from, Annotations to)
 => Traversal (Type from) (Type to) (TypeF from Identifier) (TypeF to Identifier)
typeVariables f = _Type $ either (review typeF <$>) ((fmap.fmap) Polymorphic . f) . traverse typeVariable where
  typeVariable (Polymorphic name) = Right name
  typeVariable (Function arity a b) = Left $ Function arity <$> typeVariables f a <*> typeVariables f b
  typeVariable ty = Left . pure $ unsafeCoerce ty

{- | Traversal mapping checked type variables to free type variables. -}
free :: Traversal (Type Checked) (Type Checking) Identifier Identifier
free f = typeVariables $ \(Checked name) -> Free <$> f name

{- | Traversal mapping checked type variables to checking type variables. -}
uncheck :: Traversal (Type Checked) (Type Checking) Identifier Identifier
uncheck f = typeVariables $ \(Checked name) -> Bound <$> f name

{- | Traversal mapping free type variables to checked type variables. -}
bind :: Traversal (Type Checking) (Type Checked) Identifier Identifier
bind f = typeVariables $ fmap Checked . f . \case Free name -> name; Bound name -> name

types :: Annotations ann => Traversal' (Type ann) (Type ann)
types = dataType . dataTypes

dataTypes :: Traversal (DataType (Type from)) (DataType (Type to)) (Type from) (Type to)
dataTypes f = \case
  Function arity a b -> Function arity <$> f a <*> f b
  ty -> pure $ unsafeCoerce ty

boxed :: DataType ty -> DataType ty
boxed (Unboxed ty) = Boxed ty
boxed a = a


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

instance (Show (Type ann), Annotations ann) => Show (DataType (Type ann)) where
  show (Unboxed ty) = show ty
  show (Boxed ty) = "{" ++ show ty ++ "}"
  show (ADT name) = show name ++ "#"
  show (Function ar arg ret) = case arg ^? dataType of
    Just Function{} -> "(" ++ show arg ++ ")" ++ show ar ++ show ret
    _ -> show arg ++ show ar ++ show ret
  show (Polymorphic name) = show name

instance Show Primitive where
  show Integer = "Int"
  show Float = "Float"


-- * Typed

class Typed ann a | a -> ann where
  typeOf :: a -> Type ann

instance (Comonad f, Annotations ann) => Typed ann (Definition ann f) where
  typeOf (Definition _ e) = typeOf $ extract e
  typeOf (Constructor _ typeName _) = dataType # ADT (extract typeName)

instance (Comonad f, Annotations ann) => Typed ann (Expression ann f) where
  typeOf (Literal (IntegerLiteral _)) = dataType # Unboxed Integer
  typeOf (Literal (FloatLiteral _)) = dataType # Unboxed Float
  typeOf (Reference _ _ _ ty) = ty
  typeOf (Lambda args expr) = go (length args) args where
    go _ [] = typeOf $ extract expr
    go m (a:as) = dataType # Function (Arity m) (typeOf $ extract a) (go (m-1) as)
  typeOf (Apply _ _ ty) = ty

instance Typed ann (Arg ann) where
  typeOf (Arg _ ty) = ty

{- | Traversal over all type annotations in an expression. -}
typeAnnotations :: (Traversable f, Ref from ~ Ref to)
 => Traversal (Expression from f) (Expression to f) (Type from) (Type to)
typeAnnotations f = \case
  Literal lit -> pure $ Literal lit
  Reference kind name rep ty -> Reference kind name <$> f rep <*> f ty
  Lambda args expr -> Lambda <$> traverse (traverse $ argType f) args <*> traverse (typeAnnotations f) expr where
    argType f (Arg name ty) = Arg name <$> f ty
  Apply fun arg ty -> Apply <$> traverse (typeAnnotations f) fun <*> traverse (typeAnnotations f) arg <*> f ty

{- | Replace one type with another in an expression. -}
replaceType :: (Eq (Type ann), Traversable f) => Type ann -> Type ann -> Expression ann f -> Expression ann f
replaceType from to = typeAnnotations . filtered (== from) .~ to


-- * Bound instances

instance Bound f (Definition ann f) where
  identifier (Definition name _) = name
  identifier (Constructor name _ _) = name

instance Comonad f => Bound f (f (Definition ann f)) where
  identifier = identifier . extract

instance Functor f => Bound f (f (Arg ann)) where
  identifier = fmap $ \case Arg name _ -> name
