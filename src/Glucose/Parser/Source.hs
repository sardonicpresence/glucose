module Glucose.Parser.Source where

import Control.Comonad
import Control.Lens
import Data.Semigroup
import Data.Text as Text
import Glucose.Identifier
import Glucose.Lexer.Location

data SourceRange = SourceRange Location Location deriving (Eq, Ord)

instance Show SourceRange where
  showsPrec d (SourceRange start end) = showParen (d>10) $ shows start . showString " - " . shows end

instance Read SourceRange where
  readsPrec d s0 = [ (SourceRange start end, s3)
                   | (start, s1) <- readsPrec (d+1) s0
                   , ("-", s2) <- lex s1
                   , (end, s3) <- readsPrec (d+1) s2]

instance Semigroup SourceRange where
  SourceRange a b <> SourceRange c d = SourceRange (min a c) (max b d)

data FromSource a = FromSource SourceRange a deriving (Eq, Ord, Functor)

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

instance Foldable FromSource where
  foldMap f (FromSource _ a) = f a

instance Traversable FromSource where
  traverse f (FromSource s a) = FromSource s <$> f a

instance Bound a => Bound (FromSource a) where
  identifier (FromSource _ a) = identifier a

startLocation :: FromSource a -> Location
startLocation (FromSource (SourceRange start _) _) = start

_fromSource :: Lens' (FromSource a) a
_fromSource = lens extract ($>)

showSource :: FromSource a -> Text -> Text
showSource (FromSource (SourceRange from to) _) =
  Text.take (codePointsBetween from to + 1) . Text.drop (codePoint from)

at :: a -> String -> FromSource a
a `at` s = FromSource (read s) a
