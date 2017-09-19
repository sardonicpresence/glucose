module LLVM.Name where

import Data.Char
import Data.Monoid
import Data.String
import Data.Text as Text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (hexadecimal)

{- | An identifier for use in LLVM IR.
   Can be local or global; doesn't include the prefix.
   Non-ASCII characters are represented by their codepoint in hex (lower-case) between '$'s.
   Glucose built-ins start with a single '$'.
   Two '$'s are used as a separated to derive unique, generated names from user-defined names.
   Generated code, such as lambdas, are named based on the function/variable requiring them
   followed by two '$'s and an ordinal e.g. 'test$$2'.
   Tagged function pointers are named by suffixing '$$' to the function name.
-}
newtype Name = Name Text
  deriving (Eq)

instance Show Name where
  show (Name name) = unpack name

instance IsString Name where
  fromString = mkName . pack

mkName :: Text -> Name
mkName = Name . mangle

localName :: Int -> Name
localName = Name . pack . show

deriveName :: Name -> Int -> Name
deriveName (Name base) n = Name $ base <> "$$" <> pack (show n)

taggedName :: Name -> Name
taggedName (Name a) = Name $ a <> "$$"

mangle :: Text -> Text
mangle = Text.concatMap mangleChar where
  mangleChar c | validChar c = singleton c
  mangleChar c = toStrict . toLazyText $ "$" <> hexadecimal (ord c) <> "$"
  validChar c = isAsciiUpper c || isAsciiLower c || isDigit c || c `elem` ("-._" :: String)
