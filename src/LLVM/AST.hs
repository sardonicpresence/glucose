module LLVM.AST where

import Data.Char
import Data.Monoid
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (hexadecimal)
import qualified Data.Text as Text

data Module = Module [Global]
  deriving (Eq)
instance Show Module where
  show (Module globals) = concatMap show globals

data Global = VariableDefinition Name Expression
  deriving (Eq)
instance Show Global where
  show (VariableDefinition name value) = "@" ++ show name ++ " = unnamed_addr constant " ++ withType value ++ "\n"

data Expression = Literal Constant | GlobalReference Name Type
  deriving (Eq)
instance Show Expression where
  show (Literal value) = show value
  show (GlobalReference name _) = "@" ++ show name
instance Typed Expression where
  typeOf (Literal value) = typeOf value
  typeOf (GlobalReference _ ty) = ty
  withType (Literal value) = show (typeOf value) ++ " " ++ show value
  withType (GlobalReference name ty) = show ty ++ " getelementptr inbounds (" ++ show ty ++ ", " ++ show ty ++ " * @" ++ show name ++ ", i64 0)"

data Name = Name Text
  deriving (Eq)
instance Show Name where
  show (Name name) = Text.unpack name

mkName :: Text -> Name
mkName = Name . mangle

mangle :: Text -> Text
mangle = Text.concatMap mangleChar where
  mangleChar c | validChar c = Text.singleton c
  mangleChar c = toStrict . toLazyText $ "$" <> hexadecimal (ord c) <> "$"
  validChar c = isAsciiUpper c || isAsciiLower c || isDigit c || c `elem` ("-._" :: String)

data Constant = I32 Int | F64 Double
  deriving (Eq)
instance Show Constant where
  show (I32 n) = show n
  show (F64 n) = show n

data Type = TI32 | TF64
  deriving (Eq)
instance Show Type where
  show TI32 = "i32"
  show TF64 = "double"

class Typed a where
  typeOf :: a -> Type
  withType :: a -> String
  default withType :: Show a => a -> String
  withType a = show (typeOf a) ++ " " ++ show a

instance Typed Constant where
  typeOf (I32 _) = TI32
  typeOf (F64 _) = TF64
