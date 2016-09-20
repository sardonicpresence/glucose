module Glucose.Parser.EOFOr where

data EOFOr a = EOF | NotEOF a
  deriving (Functor, Eq, Show)

-- instance Ord a => Ord (EOFOr a) where
--   compare EOF EOF = EQ
--   compare EOF (NotEOF _) = GT
--   compare (NotEOF _) EOF = LT
--   compare (NotEOF a) (NotEOF b) = compare a b

maybeEOF :: b -> (a -> b) -> EOFOr a -> b
maybeEOF b _ EOF = b
maybeEOF _ f (NotEOF a) = f a
