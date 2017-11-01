module Glucose.IR.Format where

import Control.Comonad
import Control.Lens ((^?))
import Data.Format
import Data.Monoid ((<>))
import Data.Text as Text (pack, null, intercalate)
import Glucose.Identifier (Identifier)
import Glucose.IR

type FormatsAnnotations f ann = (Annotations ann, Formats TypeFormat f (Type ann), FormattableFunctor f (TypeF ann), FormattableFunctor f (Ref ann))

-- * Reference formatting

data ReferenceKindFormat = IdentifierOnly | AnnotatedIdentifier deriving (Eq)

instance ProvidesFormat ReferenceKindFormat f => FormattableFunctor f ReferenceKind where
  fformat f (Local a) = formatIf AnnotatedIdentifier ("%" <>) f a
  fformat f (Global a) = formatIf AnnotatedIdentifier ("@" <>) f a


-- * Type formatting

data TypeFormat = GlucoseType | CodegenType deriving (Eq)

instance FormatsAnnotations f ann => Formattable f (DataType (Type ann)) where
  format f (Unboxed ty) = format f ty
  format f (Boxed ty) = formatIf CodegenType (within "{" "}") f ty
  format f (ADT name) = formatIf CodegenType (<> "#") f name
  format f (Function arity from to) = from' <> format f arity <> format f to where
    from' = case from ^? dataType of
              Just Function{} -> "(" <> format f from <> ")"
              _ -> format f from
  format f (Polymorphic ty) = format f ty

instance ProvidesFormat TypeFormat f => Formattable f Arity where
  format f a = case (getFormat f, a) of
    (GlucoseType, Arity n) -> " -" <> pack (show n) <> "> "
    _ -> " -> "

instance Formattable f Primitive where
  format _ Integer = "Int"
  format _ Float = "Float"

instance FormattableFunctor f (TypeF Unchecked) where
  fformat _ Untyped = mempty

instance ProvidesFormat TypeFormat f => FormattableFunctor f (TypeF Checking) where
  fformat _ Any = "*"
  fformat f (Free name) = formatIf CodegenType ("*" <>) f name
  fformat f (Bound ty) = format f ty

instance FormattableFunctor f (TypeF Checked) where
  fformat f (Checked ty) = format f ty


-- * Formatting with types

data WithType ann a = WithType a (Type ann)

withType = WithType

instance (FormatsAnnotations f ann, Formattable f a) => Formattable f (WithType ann a) where
  format f (WithType a ty) = let ty' = format f ty in
    if Text.null ty' then format f a else format f a <> " : " <> ty'


-- * IR formatting

type FormatsIR f ann w = (FormatsAnnotations f ann, FormattableFunctor f w, Formattable f (Ref ann Identifier))

instance (Comonad w, FormatsIR f ann w) => Formattable f (Module ann w) where
  format f (Module defs) = Text.intercalate "\n\n" $ map (format f) defs

instance (Comonad w, FormatsIR f ann w) => Formattable f (Definition ann w) where
  format f (Definition n value) = if declaration == name then definition else declaration <> "\n" <> definition where
    name = format f n
    declaration = format f $ name `withType` typeOf (extract value)
    definition = name <> " = " <> format f value
  format f (Constructor name typeName index) = format f name <> " = " <> format f typeName <> "#" <> pack (show index)

instance FormatsIR f ann w => Formattable f (Expression ann w) where
  format _ (Literal lit) = pack $ show lit
  format f (Reference ref ty) = format f $ ref `withType` ty
  format f (Lambda arg value) = "\\" <> format f arg <> " -> " <> format f value
  format f (Apply expr arg ty) = format f $ ("(" <> format f expr <> ") (" <> format f arg <> ")") `withType` ty

instance FormatsAnnotations f ann => Formattable f (Arg ann) where
  format f (Arg name ty) = format f $ name `withType` ty

instance Show Literal where
  show (IntegerLiteral a) = show a
  show (FloatLiteral a) = show a
