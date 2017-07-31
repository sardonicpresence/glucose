module Glucose.Parser.EOFOr where

data EOFOr a = EOF | NotEOF a
  deriving (Functor, Eq, Show)

maybeEOF :: b -> (a -> b) -> EOFOr a -> b
maybeEOF b _ EOF = b
maybeEOF _ f (NotEOF a) = f a

fromEOF :: a -> EOFOr a -> a
fromEOF a = maybeEOF a id
