module Glucose.Parser.Monad where

import Control.Applicative
import Control.Lens hiding (traverse1)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State.Strict
import Data.Semigroup
import Glucose.Parser.EOFOr

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

instance Semigroup e => MonadError e (Outcome e) where
  throwError = Failure
  catchError (Failure e) f = f e
  catchError a _ = a

resolveOutcome :: MonadError e m => Outcome e a -> m a
resolveOutcome (Success a) = pure a
resolveOutcome (Failure e) = throwError e
resolveOutcome (Pure _ a) = pure a

type Parser e t ts a = (Semigroup e, Cons ts ts t t) => ReaderT (EOFOr t -> [EOFOr String] -> e) (StateT ts (Outcome e)) a

runParser :: (Semigroup e, Cons ts ts t t, MonadError e m) => Parser e t ts a -> (EOFOr t -> [EOFOr String] -> e) -> ts -> m a
runParser p onError ts = resolveOutcome $ fst <$> runStateT (runReaderT p onError) ts

parser :: (Maybe (t, ts) -> Parser e t ts a) -> Parser e t ts a
parser f = gets uncons >>= \p -> case p of
  Nothing -> f Nothing
  Just (_, ts) -> put ts *> f p

eof :: Parser e t ts ()
eof = parser $ \case
  Nothing -> pure ()
  Just (t, _) -> parseError (NotEOF t) EOF

lexeme :: String -> (t -> Maybe a) -> Parser e t ts a
lexeme label f = parser $ \case
  Nothing -> parseError EOF (NotEOF label)
  Just (t, ts') -> case f t of
    Nothing -> parseError (NotEOF t) (NotEOF label)
    Just a -> put ts' *> pure a

parseError :: EOFOr t -> EOFOr String -> Parser e t ts a
parseError u e = asks (\f -> f u [e]) >>= throwError

-- * Utilities

separatedBy :: Alternative f => f a -> f b -> f [a]
separatedBy p sep = (<|) <$> p <*> many (sep *> p)

traverse1 :: Applicative f => (a -> f b) -> [a] -> f [b]
traverse1 _ [] = errorWithoutStackTrace "traverse1: empty structure"
traverse1 f [a] = pure <$> f a
traverse1 f (a:as) = (:) <$> f a <*> traverse1 f as

sequence1 :: Applicative f => [f a] -> f [a]
sequence1 = traverse1 id
