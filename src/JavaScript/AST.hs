module JavaScript.AST where

import Data.Set
import Data.Text

{- | JavaScript source. Not an AST; just text. -}
newtype JavaScript = JavaScript Text

instance Show JavaScript where
  show (JavaScript s) = unpack s

reservedWords :: Set Text
reservedWords = fromList [
  "break", "case", "catch", "class", "const", "continue", "debugger", "default", "delete", "do", "else", "export",
  "extends", "finally", "for", "function", "if", "import", "in", "instanceof", "new", "return", "super", "switch",
  "this", "throw", "try", "typeof", "var", "void", "while", "with", "yield",
  -- Future reserved
  "await", "enum", "implements", "interface", "let", "package", "private", "protected", "public", "static" ]

isReserved :: Text -> Bool
isReserved = flip member reservedWords
