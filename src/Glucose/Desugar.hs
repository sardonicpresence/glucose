module Glucose.Desugar where

import Control.Comonad
import Control.Monad
import Glucose.AST as AST
import Glucose.Error
import Glucose.Identifier
import Glucose.IR as IR
import Glucose.Source

type Desugared t = FromSource (t Unchecked)

desugar :: Error m => AST.Module -> m (IR.Module Unchecked)
desugar (AST.Module defs) = IR.Module . concat <$> traverse definition defs

definition :: Error m => FromSource AST.Definition -> m [Desugared IR.Definition]
definition def = case extract def of
  AST.Definition name expr _ -> pure . (def $>) . IR.Definition name <$> traverse expression expr
  AST.TypeDefinition name ctors -> zipWithM (constructor name) [0..] ctors

constructor :: Error m => FromSource Identifier -> Int -> FromSource Identifier -> m (Desugared IR.Definition)
constructor typeName n ctor = pure $ ctor $> IR.Definition ctor (ctor $> IR.Constructor typeName n)

expression :: Error m => AST.Expression -> m (IR.Expression Unchecked)
expression (AST.Value a) = value a
expression (AST.Apply f a) = IR.Apply <$> traverse expression f <*> traverse value a

value :: Error m => AST.Value -> m (IR.Expression Unchecked)
value (AST.Literal lit) = IR.Literal <$> literal lit
value (AST.Variable name) = pure $ IR.Reference UnknownKind name Unknown Unknown
value (AST.Lambda args expr) = IR.Lambda <$> mapM (traverse arg) args <*> traverse expression expr

arg :: Error m => Identifier -> m (IR.Arg Unchecked)
arg a = pure $ IR.Arg a Unknown

literal :: Error m => AST.Literal -> m IR.Literal
literal (AST.IntegerLiteral value) = pure $ IR.IntegerLiteral value
literal (AST.FloatLiteral value) = pure $ IR.FloatLiteral value
