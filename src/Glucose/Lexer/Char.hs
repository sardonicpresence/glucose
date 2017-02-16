module Glucose.Lexer.Char
(
  module Data.Char,
  isNewline, isSingleSpace, isIdentifier, isOperator
)
where

import Data.Char

isNewline :: Char -> Bool
isNewline '\x2028' = True -- Line separator
isNewline '\x2029' = True -- Paragraph separator
isNewline '\x85' = True -- Next-line
isNewline '\f' = True -- Form feed
isNewline '\n' = True
isNewline _ = False

isIdentifier :: Char -> Bool
isIdentifier c = isAlphaNum c || isMark c || generalCategory c == ConnectorPunctuation

isOperator :: Char -> Bool
isOperator c = c `elem` ("~!@#$%^&|*-+=<>.?:" :: String)

isSingleSpace :: Char -> Bool
isSingleSpace ' ' = True
isSingleSpace '\xa0' = True -- Non-breaking space
isSingleSpace _ = False
