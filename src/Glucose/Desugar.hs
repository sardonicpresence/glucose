module Glucose.Desugar where

import Control.Comonad.Utils
import Control.Monad
import Glucose.AST as AST
import Glucose.Identifier
import Glucose.IR as IR

type Desugared f t = f (t Unchecked f)

desugar :: (Comonad f, Applicative m, Traversable m) => AST.Module f -> m (IR.Module Unchecked f)
desugar (AST.Module defs) = IR.Module . concat <$> traverse definition defs

definition :: (Comonad f, Applicative m, Traversable m) => f (AST.Definition f) -> m [Desugared f IR.Definition]
definition def = case extract def of
  AST.Definition name expr -> pure . (def $>) . IR.Definition name <$> mapC expression expr
  AST.TypeDefinition name ctors -> zipWithM (constructor name) [0..] ctors

constructor :: (Comonad f, Applicative m) => f Identifier -> Int -> f Identifier -> m (Desugared f IR.Definition)
constructor typeName n ctor = pure $ ctor $> IR.Constructor ctor typeName n

expression :: (Comonad f, Applicative m, Traversable m) => AST.Expression f -> m (IR.Expression Unchecked f)
expression (AST.Value a) = value a
expression (AST.Apply f a) = IR.Apply <$> mapC expression f <*> mapC value a <*> pure (Type Untyped)

value :: (Applicative m, Traversable m) => Comonad f => AST.Value f -> m (IR.Expression Unchecked f)
value (AST.Literal lit) = IR.Literal <$> literal lit
value (AST.Variable name) = pure $ IR.Reference (pure name) (Type Untyped)
value (AST.Lambda args expr) = IR.Lambda <$> mapC arg args <*> mapC expression expr

arg :: Applicative m => Identifier -> m (IR.Arg Unchecked)
arg a = pure $ IR.Arg a (Type Untyped)

literal :: Applicative m => AST.Literal -> m IR.Literal
literal (AST.IntegerLiteral value) = pure $ IR.IntegerLiteral value
literal (AST.FloatLiteral value) = pure $ IR.FloatLiteral value
