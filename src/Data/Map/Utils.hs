module Data.Map.Utils (insertOr, insertOrLookup) where

import Control.Lens
import Data.Map
import Data.Maybe

insertOr :: (Applicative f, Ord k) => k -> v -> (v -> f (Map k v)) -> Map k v -> f (Map k v)
insertOr k v onExisting m =
  case insertLookupWithKey noReplace k v m of
    (Nothing, m') -> pure m'
    (Just existing, _) -> onExisting existing

insertOrLookup :: Ord k => k -> v -> Map k v -> (v, Map k v)
insertOrLookup k v m = insertLookupWithKey noReplace k v m & _1 %~ fromMaybe v

noReplace :: k -> v -> v -> v
noReplace _ _ a = a
