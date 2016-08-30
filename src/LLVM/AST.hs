module LLVM.AST where

import Data.Char
import Data.List
import Data.Monoid
import Data.Text (Text)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (hexadecimal)
import qualified Data.Text as Text

data Module = Module [Global]
  deriving (Eq)
instance Show Module where
  show (Module globals) = intercalate "\n" $ map show globals

data Global = VariableDefinition Name Constant
  deriving (Eq)
instance Show Global where
  show (VariableDefinition name value) = "@" ++ show name ++ " = unnamed_addr constant " ++ withType value ++ "\n"

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

instance Typed Constant where
  typeOf (I32 _) = TI32
  typeOf (F64 _) = TF64

withType :: (Show a, Typed a) => a -> String
withType a = show (typeOf a) ++ " " ++ show a
