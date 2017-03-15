module Glucose.Parser.Monad where

import Control.Applicative
import Control.Lens hiding (traverse1)
import Control.Monad.Throw
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Function
import Data.Semigroup
import Glucose.Parser.EOFOr

data ParseError l t = ParseError { location :: l, unexpected :: EOFOr t, expected :: [EOFOr String] }

instance Ord l => Semigroup (ParseError l t) where
  a <> b | location a > location b = a
  a <> b | location b > location a = b
  a <> b = ParseError (location a) (unexpected a) (((<>) `on` expected) a b)

data Outcome e a = Failure e | Success a | Pure (Maybe e) a deriving (Show)

instance Functor (Outcome e) where
  fmap f (Success a) = Success (f a)
  fmap f (Pure e a) = Pure e (f a)
  fmap _ (Failure e) = Failure e

instance Semigroup e => Applicative (Outcome e) where
  pure = Pure Nothing
  (Failure e) <*> _ = Failure e
  (Pure e1 _) <*> (Failure e2) = Failure (maybe e2 (<> e2) e1)
  _ <*> (Failure e) = Failure e
  (Success f) <*> (Success a) = Success (f a)
  (Pure _ f) <*> (Success a) = Success (f a)
  (Success f) <*> (Pure e a) = Pure e (f a)
  (Pure e1 f) <*> (Pure e2 a) = Pure ((<>) <$> e1 <*> e2 <|> e1 <|> e2) (f a)

instance Semigroup e => Alternative (Outcome e) where
  empty = Failure undefined
  (Pure e a) <|> _ = Pure e a
  (Success a) <|> _ = Success a
  _ <|> (Success b) = Success b
  (Failure a) <|> (Failure b) = Failure (a <> b)
  (Failure e1) <|> (Pure e2 a) = Pure (Just $ maybe e1 (e1 <>) e2) a

instance Semigroup e => Monad (Outcome e) where
  (Failure e) >>= _ = Failure e
  (Pure e a) >>= f = case f a of
    Failure e2 -> Failure (maybe e2 (<> e2) e)
    Success b -> Pure e b
    Pure e2 b -> Pure ((<>) <$> e <*> e2 <|> e <|> e2) b
  (Success a) >>= f = f a

instance Semigroup e => MonadPlus (Outcome e)

instance Semigroup e => MonadThrow e (Outcome e) where
  throwError = Failure

liftOutcome :: MonadThrow e m => Outcome e a -> m a
liftOutcome (Success a) = pure a
liftOutcome (Failure e) = throwError e
liftOutcome (Pure _ a) = pure a

type ParseOutcome l t = Outcome (ParseError l t)

type Parser l t ts a = ReaderT (EOFOr t -> l) (StateT ts (ParseOutcome l t)) a

runParser :: MonadThrow (ParseError l t) m => Parser l t ts a -> (EOFOr t -> l) -> ts -> m a
runParser p fLocation ts = liftOutcome $ fst <$> runStateT (runReaderT p fLocation) ts

parser :: (Ord l, Cons ts ts t t) => (Maybe (t, ts) -> Parser l t ts a) -> Parser l t ts a
parser f = gets uncons >>= \case
  Nothing -> f Nothing
  Just (t, ts) -> put ts *> f (Just (t, ts))

eof :: (Ord l, Cons ts ts t t) => Parser l t ts ()
eof = parser $ \case
  Nothing -> pure ()
  Just (t, _) -> parseError (NotEOF t) EOF

lexeme :: (Ord l, Cons ts ts t t) => String -> (t -> Maybe a) -> Parser l t ts a
lexeme label f = parser $ \case
  Nothing -> parseError EOF (NotEOF label)
  Just (t, ts') -> case f t of
    Nothing -> parseError (NotEOF t) (NotEOF label)
    Just a -> put ts' *> pure a

parseError :: Ord l => EOFOr t -> EOFOr String -> Parser l t ts a
parseError u e = asks ($ u) >>= \l -> throwError $ ParseError l u [e]

-- * Utilities

separatedBy :: Alternative f => f a -> f b -> f [a]
separatedBy p sep = (<|) <$> p <*> many (sep *> p)

traverse1 :: Applicative f => (a -> f b) -> [a] -> f [b]
traverse1 _ [] = errorWithoutStackTrace "traverse1: empty structure"
traverse1 f [a] = pure <$> f a
traverse1 f (a:as) = (:) <$> f a <*> traverse1 f as

sequence1 :: Applicative f => [f a] -> f [a]
sequence1 = traverse1 id
