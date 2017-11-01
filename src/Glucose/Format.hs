module Glucose.Format (module Data.Format, module Glucose.Format) where

import Data.Format
import Data.Text (unpack)
import Glucose.IR.Format (ReferenceKindFormat(..), TypeFormat(..))
import Glucose.Source (FromSourceFormat(..))

data Format = User | Complete

instance {-# OVERLAPPABLE #-} Formattable Format a => Show a where
  show = unpack . format Complete

instance ProvidesFormat ReferenceKindFormat Format where
  getFormat User = IdentifierOnly
  getFormat Complete = AnnotatedIdentifier

instance ProvidesFormat TypeFormat Format where
  getFormat User = GlucoseType
  getFormat Complete = CodegenType

instance ProvidesFormat FromSourceFormat Format where
  getFormat _ = WithoutSource
