{-# LANGUAGE TypeFamilies, FunctionalDependencies, PatternSynonyms #-}
module Glucose.IR
(
  Module(..), Definition(..), Expression(..), Literal(..), Arg(..),
  Annotations(..), Unchecked, Checking, Checked, Type(..), TypeF(..),
  Primitive(..), DataType(..), Arity(..), ReferenceKind(..),
  pattern AnyType, pattern BoundType, pattern FreeType, pattern CheckedType,
  dataType, typeVariables, free, uncheck, bind, types, bindings, boxed, unboxed, freeTypes, bindTypes, remap,
  Typed(..), typeAnnotations, replaceType, argType
)
where

import Control.Comonad
import Control.Generalised
import Control.Lens
import Data.Format
import Data.List
import Data.Maybe
import Data.String
import Data.Traversable
import Glucose.Identifier
import Unsafe.Coerce

newtype Module ann f = Module [f (Definition ann f)]
deriving instance (Eq (f (Definition ann f))) => Eq (Module ann f)

data Definition ann f = Definition (f Identifier) (f (Expression ann f))
                      | Constructor (f Identifier) (f Identifier) Int
deriving instance (Eq (f Identifier), Eq (f (Expression ann f))) => Eq (Definition ann f)

data Expression ann f
  = Literal Literal
  | Reference (Ref ann Identifier) (Type ann)
  | Lambda (f (Arg ann)) (f (Expression ann f))
  | Apply (f (Expression ann f)) (f (Expression ann f)) (Type ann)
deriving instance (Eq (Type ann), Eq (Ref ann Identifier), Eq (f Identifier), Eq (f (Arg ann)), Eq (f (Expression ann f))) => Eq (Expression ann f)

data Literal = IntegerLiteral Int | FloatLiteral Double deriving (Eq)

data Arg ann = Arg Identifier (Type ann)
deriving instance Eq (Type ann) => Eq (Arg ann)
deriving instance Ord (Type ann) => Ord (Arg ann)


-- * Annotations

class Traversable (TypeF ann) => Annotations ann where
  data TypeF ann :: * -> *
  type Ref ann :: * -> *
  typeF :: Prism' (TypeF ann a) a

newtype Type ann = Type (TypeF ann (DataType (Type ann)))

deriving instance Eq (TypeF ann (DataType (Type ann))) => Eq (Type ann)
deriving instance Ord (TypeF ann (DataType (Type ann))) => Ord (Type ann)

instance Formattable f (TypeF ann (DataType (Type ann))) => Formattable f (Type ann) where
  format f (Type ty) = format f ty

_Type :: Iso (Type from) (Type to) (TypeF from (DataType (Type from))) (TypeF to (DataType (Type to)))
_Type = flip iso Type $ \(Type ty) -> ty

dataType :: Annotations ann => Prism' (Type ann) (DataType (Type ann))
dataType = _Type . typeF

data Unchecked
data Checking
data Checked


-- * References

data ReferenceKind a = Local a | Global a deriving (Eq, Functor)

instance Comonad ReferenceKind where
  extract (Local a) = a
  extract (Global a) = a
  duplicate (Local a) = Local (Local a)
  duplicate (Global a) = Global (Global a)


-- * Types

data Primitive = Integer | Float
  deriving (Eq, Ord)

data Arity = UnknownArity | Arity Int
  deriving (Eq, Ord)

data DataType t = Unboxed Primitive | Boxed Primitive | ADT Identifier | Function Arity t t | Polymorphic Identifier
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance IsString (DataType t) where
  fromString = Polymorphic . fromString

instance Annotations Unchecked where
  data TypeF Unchecked ty = Untyped
    deriving (Eq, Functor, Foldable, Traversable)
  type Ref Unchecked = Identity
  typeF = prism (const Untyped) Left

instance Annotations Checking where
  data TypeF Checking ty = Any | Free Identifier | Bound ty
    deriving (Eq, Functor, Foldable, Traversable)
  type Ref Checking = ReferenceKind
  typeF = prism' Bound $ \case Bound ty -> Just ty; _ -> Nothing

instance Annotations Checked where
  newtype TypeF Checked ty = Checked ty
    deriving (Eq, Ord, Functor, Foldable, Traversable)
  type Ref Checked = ReferenceKind
  typeF = prism' Checked $ \(Checked ty) -> Just ty

pattern AnyType :: Type Checking
pattern AnyType = Type Any

pattern FreeType :: Identifier -> Type Checking
pattern FreeType name = Type (Free name)

pattern BoundType :: DataType (Type Checking) -> Type Checking
pattern BoundType ty = Type (Bound ty)

pattern CheckedType :: DataType (Type Checked) -> Type Checked
pattern CheckedType ty = Type (Checked ty)

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

{- | Traversal mapping free type variables & 'any' types to checked type variables. -}
bind :: Traversal (Type Checking) (Type Checked) (Maybe Identifier) Identifier
bind f = typeVariables $ fmap Checked . f . \case Any -> Nothing; Free name -> Just name; Bound name -> Just name

types :: Annotations ann => Traversal' (Type ann) (Type ann)
types = dataType . dataTypes

dataTypes :: Traversal (DataType (Type from)) (DataType (Type to)) (Type from) (Type to)
dataTypes f = \case
  Function arity a b -> Function arity <$> f a <*> f b
  ty -> pure $ unsafeCoerce ty

boxed :: DataType ty -> DataType ty
boxed (Unboxed ty) = Boxed ty
boxed a = a

unboxed :: DataType ty -> DataType ty
unboxed (Boxed ty) = Unboxed ty
unboxed a = a

freeTypes :: (Applicative m, Traversable f) => m Identifier -> Expression Checked f -> m (Expression Checking f)
freeTypes = remap $ typeAnnotations . free

bindTypes :: (Applicative m, Traversable f) => m Identifier -> Expression Checking f -> m (Expression Checked f)
bindTypes = remap $ typeAnnotations . bind

remap :: (Eq a, Applicative m) => Traversal from to a Identifier -> m Identifier -> from -> m to
remap remapping newName from = do
  let names = nub $ from ^.. getting remapping
  subs <- for names $ \name -> (name, ) <$> newName
  pure $ from & remapping %~ \a -> fromJust $ lookup a subs


-- * Typed

class Typed ann a | a -> ann where
  typeOf :: a -> Type ann

instance (Comonad f, Annotations ann) => Typed ann (Definition ann f) where
  typeOf (Definition _ e) = typeOf $ extract e
  typeOf (Constructor _ typeName _) = dataType # ADT (extract typeName)

instance (Comonad f, Annotations ann) => Typed ann (Expression ann f) where
  typeOf (Literal (IntegerLiteral _)) = dataType # Unboxed Integer
  typeOf (Literal (FloatLiteral _)) = dataType # Unboxed Float
  typeOf (Reference _ ty) = ty
  typeOf (Lambda (pure -> args) expr) = go (length args) args where
    go _ [] = typeOf (extract expr) & dataType %~ unboxed
    go m (a:as) = dataType # Function (Arity m) (typeOf $ extract a) (go (m-1) as)
  typeOf (Apply _ _ ty) = ty

instance Typed ann (Arg ann) where
  typeOf (Arg _ ty) = ty

{- | Traversal over all type annotations in an expression. -}
typeAnnotations :: (Traversable f, Ref from ~ Ref to)
 => Traversal (Expression from f) (Expression to f) (Type from) (Type to)
typeAnnotations f = \case
  Literal lit -> pure $ Literal lit
  Reference ref ty -> Reference ref <$> f ty
  Lambda arg expr -> Lambda <$> traverse (argType f) arg <*> traverse (typeAnnotations f) expr where
    argType f (Arg name ty) = Arg name <$> f ty
  Apply fun arg ty -> Apply <$> traverse (typeAnnotations f) fun <*> traverse (typeAnnotations f) arg <*> f ty

{- | Replace one type with another in an expression. -}
replaceType :: (Eq (Type ann), Traversable f) => Type ann -> Type ann -> Expression ann f -> Expression ann f
replaceType from to = typeAnnotations . filtered (== from) .~ to

argType :: Lens (Arg from) (Arg to) (Type from) (Type to)
argType = lens (\(Arg _ ty) -> ty) (\(Arg name _) ty -> Arg name ty)


-- * Bound instances

instance Bound f (Definition ann f) where
  identifier (Definition name _) = name
  identifier (Constructor name _ _) = name

instance Comonad f => Bound f (f (Definition ann f)) where
  identifier = identifier . extract

instance Functor f => Bound f (f (Arg ann)) where
  identifier = fmap $ \case Arg name _ -> name


-- * Generalised instances

instance Generalised (Module ann) where
  rewrap remap (Module defs) = Module $ map (remap $ rewrap remap) defs

instance Generalised (Definition ann) where
  rewrap remap (Definition name def) = Definition (remap id name) (remap (rewrap remap) def)
  rewrap remap (Constructor name typeName index) = Constructor (remap id name) (remap id typeName) index

instance Generalised (Expression ann) where
  rewrap _ (Literal literal) = Literal literal
  rewrap _ (Reference ref ty) = Reference ref ty
  rewrap remap (Lambda arg expr) = Lambda (remap id arg) (remap (rewrap remap) expr)
  rewrap remap (Apply f a ty) = Apply (remap (rewrap remap) f) (remap (rewrap remap) a) ty
