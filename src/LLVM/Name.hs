module LLVM.Name where

import Data.Char
import Data.Monoid
import Data.String
import Data.Text as Text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (hexadecimal)

{- | An identifier for use in LLVM IR.
   Can be local or global based on context; doesn't include the prefix.
   Non-ASCII characters are represented by their codepoint in hex (lower-case) between '$'s.
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

mangle :: Text -> Text
mangle = Text.concatMap mangleChar where
  mangleChar c | validChar c = singleton c
  mangleChar c = toStrict . toLazyText $ "$" <> hexadecimal (ord c) <> "$"
  validChar c = isAsciiUpper c || isAsciiLower c || isDigit c || c `elem` ("-._" :: String)
