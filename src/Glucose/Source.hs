module Glucose.Source where

import Control.Applicative
import Control.Comonad
import Control.Lens
import Data.Format
import Data.Semigroup
import Data.Text as Text (Text, take, drop)

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

-- | An inclusive range of characters in UTF8 text.
data SourceRange = SourceRange Location Location deriving (Eq, Ord)

instance Show SourceRange where
  showsPrec _ (SourceRange start end) = if start == end
    then shows start
    else shows start . showString "-" . shows end

instance Read SourceRange where
  readsPrec d s0 = do
    (start, s1) <- readsPrec (d+1) s0
    let ends = do ("-", s2) <- lex s1
                  readsPrec (d+1) s2
    map (_1 %~ SourceRange start) $ ends <|> [(start, s1)]

instance Semigroup SourceRange where
  SourceRange a b <> SourceRange c d = SourceRange (min a c) (max b d)


-- | Functor associating a value with a range of chracters.
data FromSource a = FromSource SourceRange a deriving (Eq, Ord, Functor, Foldable, Traversable)

data FromSourceFormat = WithSource | WithoutSource deriving (Eq)

instance ProvidesFormat FromSourceFormat f => FormattableFunctor f FromSource where
  fformat f (FromSource loc a) = formatIf WithSource (<> " `at` " <> format f (show loc)) f a

instance Show a => Show (FromSource a) where
  showsPrec d (FromSource loc a) = showParen (d > 1) $ shows a . showString " `at` " . shows loc

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
