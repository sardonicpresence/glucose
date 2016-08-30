module Glucose.Lexer.Location where

import Data.Char
import Glucose.Lexer.Char

data Location = Location Int Int Int deriving (Eq, Ord)

instance Show Location where
  show (Location _ line col) = show line ++ ":" ++ show col

beginning :: Location
beginning = Location 0 1 1

updateLocation :: Char -> Location -> Location
updateLocation c (Location char line _) | isNewline c = Location (char+1) (line+1) 1
updateLocation c (Location char line col) | isControl c = Location (char+1) line col
updateLocation _ (Location char line col) = Location (char+1) line (col+1)

codePointsBetween :: Location -> Location -> Int
codePointsBetween (Location start _ _) (Location end _ _) = end - start
