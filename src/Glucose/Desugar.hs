module Glucose.Desugar where

import Control.Comonad
import Control.Monad
import Glucose.AST as AST
import Glucose.Error
import Glucose.Identifier
import Glucose.IR as IR
import Glucose.Parser.Source

type Desugared t = FromSource (t Unchecked)

desugar :: Error m => AST.Module -> m (IR.Module Unchecked)
desugar (AST.Module defs) = IR.Module . concat <$> traverse definition defs

definition :: Error m => FromSource AST.Definition -> m [Desugared IR.Definition]
definition def = case extract def of
  AST.Definition name expr -> pure . (def $>) . IR.Definition name <$> expression expr
  AST.TypeDefinition name ctors -> zipWithM (constructor name) [0..] ctors

constructor :: Error m => FromSource Identifier -> Int -> FromSource Identifier -> m (Desugared IR.Definition)
constructor typeName n ctor = pure $ ctor $> IR.Definition ctor (ctor $> IR.Constructor typeName n)

expression :: Error m => FromSource AST.Expression -> m (Desugared IR.Expression)
expression = traverse $ \case
  AST.Literal lit -> IR.Literal <$> literal lit
  AST.Variable name -> pure $ IR.Reference UnknownKind name Unknown

literal :: Error m => AST.Literal -> m IR.Literal
literal (AST.IntegerLiteral value) = pure $ IR.IntegerLiteral value
literal (AST.FloatLiteral value) = pure $ IR.FloatLiteral value
