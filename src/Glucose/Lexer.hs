module Glucose.Lexer where

import Data.Char
import Data.Text.Lazy (Text, unpack)
import qualified Data.Text.Lazy as Text

data Token = NotWhitespace String

tokenize :: Text -> [Token]
tokenize = concatMap (go "" . unpack) . Text.lines . normaliseNewlines where
  go "" [] = []
  go s [] = [NotWhitespace (reverse s)]
  go "" (c:cs) | isSpace c = go "" cs
               | otherwise = go [c] cs
  go s (c:cs) | isSpace c = NotWhitespace (reverse s) : go "" cs
              | otherwise = go (c:s) cs

isSingleSpace :: Char -> Bool
isSingleSpace ' ' = True
isSingleSpace '\xa0' = True
isSingleSpace '\x85' = True
isSingleSpace _ = False

normaliseNewlines :: Text -> Text
normaliseNewlines = Text.replace "\x85" "\n" . Text.replace "\f" "\n" . Text.replace "\r\n" "\n"
