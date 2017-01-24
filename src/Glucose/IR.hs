module Glucose.IR where

import Control.Comonad
import Control.Lens
import Data.List
import Data.Monoid
import qualified Data.Set as Set
import Glucose.Identifier
import Glucose.Parser.Source

data Module = Module [FromSource Definition] deriving (Eq)

data Definition = Definition (FromSource Identifier) (FromSource Expression) deriving (Eq)

data Expression
  = Literal Literal
  | Reference ReferenceKind Identifier Type
  | Lambda [FromSource Arg] (FromSource Expression)
  | Apply (FromSource Expression) (FromSource Expression)
  deriving (Eq)

data ReferenceKind = Local | Global deriving (Eq)

data Literal = IntegerLiteral Int | FloatLiteral Double deriving (Eq)

data Arg = Arg Identifier Type deriving (Eq, Ord)

data Type = Integer | Float | Function Arity Type Type | Bound Identifier
          | Free Identifier -- Must be eliminated by type-checking (should be made seperate)
  deriving (Eq, Ord)

data Arity = UnknownArity | Arity Int Int deriving (Eq, Ord)

instance Show Type where
  show Integer = "Int"
  show Float = "Float"
  show (Function ar arg@Function{} ret) = "(" ++ show arg ++ ")" ++ show ar ++ show ret
  show (Function ar arg ret) = show arg ++ show ar ++ show ret
  show (Bound name) = show name
  show (Free name) = "*" ++ show name

instance Show Arity where
  show _ = "->"
  -- show UnknownArity = "-?>"
  -- show (Arity n 0) = "-" ++ show n ++ ">"
  -- show (Arity n m) = "-" ++ show n ++ "/" ++ show m ++ ">"

argTypes :: Type -> [Type]
argTypes (Function _ a b) = a : argTypes b
argTypes _ = []

-- * Apply

-- | Chain of function applications with an optional trailing partial application
data Application = Application Expression [[Expression]] (Maybe Partial) deriving (Show)
data Partial = Partial Type [Expression] deriving (Show)

flattenApply :: Expression -> Expression -> Application
flattenApply f a = go f [a] where
  go f args = case f of
    Apply (extract -> g) (extract -> x) -> go g (x:args)
    Reference _ _ ty -> uncurry (Application f) (apply ty [] args)
    Lambda _ _ -> undefined
    Literal _ -> error "Cannot supply arguments to a literal!" -- TODO: improve & move
  -- apply :: Type -> [Expression] -> ([[Expression]], Maybe [Expression])
  apply (Function (Arity n m) _ b) applied (a:as) | n == m+1 = apply b [] as & _1 %~ (reverse (a:applied) :)
  apply (Function (Arity _ _) _ b) applied (a:as) = apply b (a:applied) as
  apply _ [] [] = ([], Nothing)
  apply b applied [] = ([], Just . Partial b $ reverse applied)
  apply _ _ _ = error "Cannot supply arguments to a non-function!" -- TODO: improve & move

-- * Type lensing

free :: Prism' Type Identifier
free = prism' Free $ \case Free a -> Just a; _ -> Nothing

bound :: Prism' Type Identifier
bound = prism' Bound $ \case Bound a -> Just a; _ -> Nothing

types :: Traversal' Expression Type
types f a@Literal{} = f (typeOf a) $> a
types f (Reference kind name ty) = Reference kind name <$> prims f ty
types f (Lambda args expr) = Lambda <$> traverse (traverse $ argType f) args <*> traverse (types f) expr where
  argType f (Arg name ty) = Arg name <$> prims f ty
types f (Apply fun arg) = Apply <$> traverse (types f) fun <*> traverse (types f) arg

prims :: Traversal' Type Type
prims f (Function rep a b) = Function rep <$> prims f a <*> prims f b
prims f a = f a

remapTypes :: Monad m => m Identifier -> Prism' Type Identifier -> Prism' Type Identifier -> Expression -> m Expression
remapTypes replacement from to expr = do
  subs <- traverse (\a -> (a ^. re from,) <$> replacement) . nub $ expr ^.. types . from
  expr & types %%~ \a -> pure . maybe a (^. re to) $ lookup a subs

freeTypes :: Monad m => m Identifier -> Expression -> m Expression
freeTypes newVar = remapTypes newVar bound free

bindTypes :: Monad m => m Identifier -> Expression -> m Expression
bindTypes newVar = remapTypes newVar free bound

bindType :: Identifier -> Type -> Expression -> Expression
bindType name = set $ types . filtered (== Free name)

rebindType :: Identifier -> Type -> Expression -> Expression
rebindType name = set $ types . filtered (== Bound name)

captures :: Expression -> Set.Set Arg
captures (Reference Local name ty) = Set.singleton $ Arg name ty
captures (Apply expr arg) = captures (extract expr) <> captures (extract arg)
captures (Lambda args value) = captures (extract value) Set.\\ Set.fromList (map extract args)
captures _ = mempty

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
  show (Lambda args value) = "\\" ++ unwords (map (show.extract) args) ++ " -> " ++ show (extract value)
  show (Apply expr arg) = show (extract expr) ++ " (" ++ show (extract arg) ++ ")"

instance Show ReferenceKind where
  show Local = "%"
  show Global = "@"

instance Show Arg where
  show (Arg name ty) = show name ++ ":" ++ show ty

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
  typeOf (Lambda args expr) = go 0 args where
    go _ [] = typeOf $ extract expr
    go m (a:as) = Function (Arity n m) (typeOf $ extract a) $ go (m+1) as
    n = length args
  typeOf (Apply f _) = case typeOf f of Function _ _ b -> b; _ -> error "Cannot pass argument to a non-function!"

instance Typed Arg where
  typeOf (Arg _ ty) = ty

instance Typed Literal where
  typeOf (IntegerLiteral _) = Integer
  typeOf (FloatLiteral _) = Float

instance Typed a => Typed (FromSource a) where
  typeOf (FromSource _ a) = typeOf a

-- * Bound instances

instance Bound Definition where
  identifier (Definition (FromSource _ name) _) = name

instance Bound Arg where
  identifier (Arg name _) = name
