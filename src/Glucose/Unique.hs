module Glucose.Unique (Unique, mkUnique, nextUnique) where

newtype Unique = Unique Int deriving (Eq, Ord)

mkUnique :: Unique
mkUnique = Unique 0

nextUnique :: Unique -> Unique
nextUnique (Unique a) = Unique (a+1)

instance Enum Unique where
  toEnum = Unique
  fromEnum (Unique a) = a
