module LLVM.Name where

import Data.Char
import Data.Monoid
import Data.Text as Text
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Text.Lazy.Builder.Int (hexadecimal)

data Name = Name Text
  deriving (Eq)

instance Show Name where
  show (Name name) = unpack name

mkName :: Text -> Name
mkName = Name . mangle

localName :: Int -> Name
localName = Name . pack . show

deriveName :: Name -> Int -> Name
deriveName (Name base) n = Name$ base <> "$$" <> pack (show n)

mangle :: Text -> Text
mangle = Text.concatMap mangleChar where
  mangleChar c | validChar c = singleton c
  mangleChar c = toStrict . toLazyText $ "$" <> hexadecimal (ord c) <> "$"
  validChar c = isAsciiUpper c || isAsciiLower c || isDigit c || c `elem` ("-._" :: String)
