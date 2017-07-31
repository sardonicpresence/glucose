module Glucose.Source where

import Control.Comonad
import Data.Semigroup
import Data.Text as Text

-- | A location in UTF8 text. Zero-based code-point and one-based line/column.
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

-- | The first character in a UTF8 document.
beginning :: Location
beginning = Location 0 1 1

-- | The number of code-points between 2 locations.
codePointsBetween :: Location -> Location -> Int
codePointsBetween (Location start _ _) (Location end _ _) = end - start

-- | Rewind a location to the previous code-point & column.
-- It as an error to attempt to rewind past the beginning of a line.
rewind :: Location -> Location
rewind (Location _ _ 1) = error "Can't rewind a Location past a newline!"
rewind (Location cp line col) = Location (cp-1) line (col-1)


-- | A value with an associated location in UTF8 text.
data Located a = Located Location a deriving (Eq, Ord, Functor, Foldable, Traversable)

location :: Located a -> Location
location (Located loc _) = loc

instance Comonad Located where
  extract (Located _ a) = a
  extend f x@(Located loc _) = Located loc (f x)

instance Semigroup a => Semigroup (Located a) where
  a <> b | location a > location b = a
  a <> b | location b > location a = b
  (Located loc a) <> (Located _ b) = Located loc (a <> b)


-- | An inclusive range of characters in UTF8 text.
data SourceRange = SourceRange Location Location deriving (Eq, Ord)

instance Show SourceRange where
  showsPrec d (SourceRange start end) = showParen (d>10) $ shows start .
                                        if start /= end then showString "-" . shows end else id

instance Read SourceRange where
  readsPrec d s0 = [ (SourceRange start end, s3)
                   | (start, s1) <- readsPrec (d+1) s0
                   , ("-", s2) <- lex s1
                   , (end, s3) <- readsPrec (d+1) s2]

instance Semigroup SourceRange where
  SourceRange a b <> SourceRange c d = SourceRange (min a c) (max b d)


-- | Functor associating a value with a range of chracters.
data FromSource a = FromSource SourceRange a deriving (Eq, Ord, Functor, Foldable, Traversable)

instance Show a => Show (FromSource a) where
  showsPrec d (FromSource loc a) = showParen (d>10) $ shows a . showString " at " . shows loc

instance Applicative FromSource where
  pure = FromSource undefined
  (FromSource r1 f) <*> (FromSource r2 a) = FromSource (r1 <> r2) (f a)

instance Monad FromSource where
  (FromSource s a) >>= f = case f a of FromSource t b -> FromSource (s <> t) b

instance Comonad FromSource where
  extract (FromSource _ a) = a
  extend f x@(FromSource s _) = FromSource s (f x)

instance ComonadApply FromSource

startLocation :: FromSource a -> Location
startLocation (FromSource (SourceRange start _) _) = start

endLocation :: FromSource a -> Location
endLocation (FromSource (SourceRange _ end) _) = end

showSource :: FromSource a -> Text -> Text
showSource (FromSource (SourceRange from to) _) =
  Text.take (codePointsBetween from to + 1) . Text.drop (codePoint from)

at :: a -> String -> FromSource a
a `at` s = FromSource (read s) a
