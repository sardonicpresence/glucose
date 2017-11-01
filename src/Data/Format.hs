module Data.Format where

import Control.Monad.Identity
import Data.Monoid ((<>))
import Data.Text as Text (Text, pack)

-- * Formattable class

class Formattable format a where
  format :: format -> a -> Text

instance Formattable format Text where
  format _ = id

instance Formattable format String where
  format _ = pack


-- * FormattableFunctor class

class Functor f => FormattableFunctor format f where
  fformat :: Formattable format a => format -> f a -> Text

instance FormattableFunctor format Identity where
  fformat f (Identity a) = format f a

instance {-# OVERLAPPABLE #-} (FormattableFunctor format f, Formattable format a) => Formattable format (f a) where
  format = fformat


-- * ProvidesFormat class

class ProvidesFormat f a where
  getFormat :: a -> f

type Formats format f a = (ProvidesFormat format f, Formattable f a)


-- * Utilities

within :: Text -> Text -> Text -> Text
within l r a = l <> a <> r

formatIf :: (Eq format, Formats format f a) => format -> (Text -> Text) -> f -> a -> Text
formatIf fmt g f a = (if getFormat f == fmt then g else id) $ format f a
