{-# LANGUAGE ScopedTypeVariables, TypeFamilies, DataKinds, AllowAmbiguousTypes #-}
module Glucose.IR
(
  Module(..), Definition(..), Expression(..), Literal(..), Arg(..),
  Annotations(..), ReferenceAnnotation(..), Unchecked, Checking, Checked, Type(..),
  Primitive(..), DataType(..), Arity(..), ReferenceKind(..),
  types, replaceType, freeTypes, bindTypes, uncheck
)
where

import Control.Comonad
import Control.Comonad.Utils
import Control.Lens
import Data.Function (fix)
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

data Unchecked
data Checking
data Checked


-- * Types

data Primitive = Integer | Float
  deriving (Eq, Ord)

data DataType t = Unboxed Primitive | Boxed Primitive | ADT Identifier | Function Arity t t | Polymorphic Identifier
  deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Annotations Unchecked where
  data Type Unchecked = Untyped
  type Ref Unchecked = ()
  withType = const

instance Annotations Checking where
  data Type Checking = Free Identifier | Bound (DataType (Type Checking))
  type Ref Checking = ReferenceKind
  name `withType` ty = name ++ ":" ++ show ty

instance Annotations Checked where
  data Type Checked = Checked (DataType (Type Checked)) deriving (Eq, Ord)
  type Ref Checked = ReferenceKind
  name `withType` ty = name ++ ":" ++ show ty

instance Show (Type Checking) where
  show (Free name) = "*" ++ show name
  show (Bound ty) = show ty

instance Show (Type Checked) where
  show (Checked ty) = show ty


-- * References

class ReferenceAnnotation a where
  showRef :: Show b => a -> b -> String

instance ReferenceAnnotation () where
  showRef _ = show

data ReferenceKind = Local | Global deriving (Eq)

instance ReferenceAnnotation ReferenceKind where
  showRef Local a = "%" ++ show a
  showRef Global a = "@" ++ show a


-- * Show instances

instance (Comonad f, Annotations ann) => Show (Module ann f) where
  show (Module defs) = intercalate "\n\n" $ map (show . extract) defs

instance (Comonad f, Annotations ann) => Show (Definition ann f) where
  show (Definition name value) =
    let n = show $ extract name
        -- declaration = show n `annotatedWith` (typeOf (extract value) :: Type ann)
        definition = show (extract name) ++ " = " ++ show (extract value)
     in definition
    --  in if declaration == n
    --       then definition
    --       else declaration ++ "\n" ++ definition
  show (Constructor name typeName index) = show (extract name) ++ " = " ++ show (extract typeName) ++ "#" ++ show index

instance (Comonad f, Annotations ann) => Show (Expression ann f) where
  show (Literal lit) = show lit
  show (Reference kind name rep ty) = showRef kind name `withType` ty `withType` rep -- TODO: include arity
  show (Lambda args value) = "\\" ++ unwords (map (show . extract) args) ++ " -> " ++ show (extract value)
  show (Apply expr arg ty) = "((" ++ show (extract expr) ++ ") (" ++ show (extract arg) ++ ")" `withType` ty ++ ")"

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

instance Show Primitive where
  show Integer = "Int"
  show Float = "Float"


-- * Types

types :: (Traversable f, Ref from ~ Ref to) => Traversal (Expression from f) (Expression to f) (Type from) (Type to)
types _ (Literal lit) = pure $ Literal lit
types f (Reference kind name rep ty) = Reference kind name <$> f rep <*> f ty
types f (Lambda args expr) = Lambda <$> traverse (traverse $ argType f) args <*> traverse (types f) expr where
  argType f (Arg name ty) = Arg name <$> f ty
types f (Apply fun arg ty) = Apply <$> traverse (types f) fun <*> traverse (types f) arg <*> f ty

replaceType :: (Eq (Type ann), Traversable f) => Type ann -> Type ann -> Expression ann f -> Expression ann f
replaceType from = set $ types . filtered (== from)

freeTypes :: (Applicative m, Traversable f) => m Identifier -> Expression Checked f -> m (Expression Checking f)
freeTypes genVar = types . fix $ \recurse -> \case
  Checked (Polymorphic _) -> Free <$> genVar
  Checked ty -> Bound <$> traverse recurse ty

bindTypes :: (Applicative m, Traversable f) => m Identifier -> Expression Checking f -> m (Expression Checked f)
bindTypes genVar = types . fix $ \recurse -> \case
  Free _ -> Checked . Polymorphic <$> genVar
  Bound ty -> Checked <$> traverse recurse ty

uncheck :: Type Checked -> Type Checking
uncheck (Checked ty) = Bound $ uncheck <$> ty

-- prims :: Traversal' Type Type
-- prims f (Function rep a b) = Function rep <$> prims f a <*> prims f b
-- prims f a = f a

-- typeOf :: Expression ann f -> Type ann
-- typeOf = const undefined

-- class Typed a ann | a -> ann where
--   typeOf :: a -> DataType (Type ann)

-- instance (Comonad f, TypeAnnotation (Type ann) (Type ann)) => Typed (Definition ann f) ann where
--   typeOf (Definition _ e) = typeOf $ extract e
--   typeOf (Constructor _ typeName _) = dataType . ADT $ extract typeName

-- instance Comonad f => Typed (Expression ann f) ann where
--   typeOf (Literal (IntegerLiteral _)) = dataType $ Primitive Integer
--   typeOf (Literal (FloatLiteral _)) = dataType $ Primitive Float
--   typeOf (Reference _ _ _ ty) = ty
--   typeOf (Lambda args expr) = go n args where
--     go _ [] = typeOf $ extract expr
--     go m (a:as) = funType (Arity m) (typeOf $ extract a) $ go (m-1) as
--     n = length args
--   typeOf (Apply f _) = returnType (typeOf $ extract f)

-- instance Typed Literal ann where
--   typeOf (IntegerLiteral _) = intType
--   typeOf (FloatLiteral _) = floatType

-- instance TypeAnnotation (Type ann) (Type ann) => Typed (Arg ann) ann where
--   typeOf (Arg _ ty) = ty

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

data Arity = UnknownArity | Arity Int deriving (Eq, Ord)

instance Show Arity where
  show _ = "->"
  -- show UnknownArity = "-?>"
  -- show (Arity n 0) = "-" ++ show n ++ ">"
  -- show (Arity n m) = "-" ++ show n ++ "/" ++ show m ++ ">"
