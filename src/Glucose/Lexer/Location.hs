module Glucose.Lexer.Location where

import Data.Char
import Glucose.Lexer.Char

data Location = Location { codePoint, line, column :: Int } deriving (Eq, Ord)

instance Show Location where
  show (Location cp line col) = show line ++ ":" ++ show col ++ "@" ++ show cp

instance Read Location where
  readsPrec d s0 = [ (Location cp line col, s5)
                   | (line, s1) <- readsPrec (d+1) s0
                   , (":", s2) <- lex s1
                   , (col, s3) <- readsPrec (d+1) s2
                   , ("@", s4) <- lex s3
                   , (cp, s5) <- readsPrec (d+1) s4]

beginning :: Location
beginning = Location 0 1 1

updateLocation :: Char -> Location -> Location
updateLocation c (Location char line _) | isNewline c = Location (char+1) (line+1) 1
updateLocation c (Location char line col) | isControl c = Location (char+1) line col
updateLocation _ (Location char line col) = Location (char+1) line (col+1)

codePointsBetween :: Location -> Location -> Int
codePointsBetween (Location start _ _) (Location end _ _) = end - start

advance :: Location -> Int -> Location
advance (Location cp line col) n = Location (cp + n) line (col + n)

rewind :: Location -> Location
rewind (Location _ _ 1) = error "Can't rewind a Location past a newline!"
rewind (Location cp line col) = Location (cp-1) line (col-1)
