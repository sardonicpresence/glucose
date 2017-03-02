module Data.Map.Utils where

import Data.Map

insertOr :: (Applicative f, Ord k) => k -> v -> (v -> f (Map k v)) -> Map k v -> f (Map k v)
insertOr k v onExisting m =
  case insertLookupWithKey noReplace k v m of
    (Nothing, m') -> pure m'
    (Just existing, _) -> onExisting existing
  where noReplace _ _ a = a
