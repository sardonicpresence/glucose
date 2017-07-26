module Glucose.Lexer.Char
(
  module Data.Char,
  isNewline, isSingleSpace, isIdentifier, isOperator, updateLocation
)
where

import Data.Char
import Glucose.Source

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

-- | Advance a location by observing a character and interpreting how it
-- changes the perceived line/column.
updateLocation :: Char -> Location -> Location
updateLocation c (Location char line _) | isNewline c = Location (char+1) (line+1) 1
updateLocation c (Location char line col) | isControl c = Location (char+1) line col
updateLocation _ (Location char line col) = Location (char+1) line (col+1)
