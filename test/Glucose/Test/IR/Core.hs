module Glucose.Test.IR.Core where

import Control.Comonad
import Data.Text (Text)
import Glucose.Identifier (Identifier (..))
import Glucose.IR
import Glucose.Parser.Source
import Glucose.Test.Source

-- * Without source locations

constantAnywhere :: Text -> Literal -> FromSource (Definition ann)
constantAnywhere name lit = definitionAnywhere name (Literal lit)

constructorAnywhere :: Text -> Text -> Int -> FromSource (Definition ann)
constructorAnywhere ty ctor index = definitionAnywhere ctor $ Constructor (fromSource $ Identifier ty) index

definitionAnywhere :: Text -> Expression ann -> FromSource (Definition ann)
definitionAnywhere name value = definition (fromSource name) (fromSource value)

-- * With source locations

constant :: FromSource Text -> FromSource Literal -> FromSource (Definition ann)
constant name lit = definition name (Literal <$> lit)

constructor :: FromSource Text -> FromSource Text -> Int -> FromSource (Definition ann)
constructor ty ctor index = definition ctor $ ctor $> Constructor (Identifier <$> ty) index

definition :: FromSource Text -> FromSource (Expression ann) -> FromSource (Definition ann)
definition name value = Definition <$> duplicate (Identifier <$> name) <*> duplicate value

reference :: RefKind ann -> FromSource Text -> Type ann -> FromSource (Expression ann)
reference kind name ty = (\n -> Reference kind (Identifier n) ty) <$> name
