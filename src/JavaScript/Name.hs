module JavaScript.Name where

import Data.Monoid ((<>))
import Data.Set (Set, fromList, member)
import Data.Text (Text, unpack)

newtype Name = Name Text
  deriving (Eq)

instance Show Name where
  show (Name n) = unpack n

mkName :: Text -> Name
mkName = Name . mangle

mangle :: Text -> Text
mangle a = if isReserved a then "$" <> a else a

reservedWords :: Set Text
reservedWords = fromList
  [ "break", "case", "catch", "class", "const", "continue", "debugger", "default", "delete", "do", "else", "export"
  , "extends", "finally", "for", "function", "if", "import", "in", "instanceof", "new", "return", "super", "switch"
  , "this", "throw", "try", "typeof", "var", "void", "while", "with", "yield"
  -- Future reserved
  , "await", "enum", "implements", "interface", "let", "package", "private", "protected", "public", "static"
  -- Literals
  , "null", "true", "false"
  ]

isReserved :: Text -> Bool
isReserved = flip member reservedWords
