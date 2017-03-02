module Data.Map.List where

import Data.Foldable (Foldable)
import Data.Traversable (Traversable, traverse)
import Data.Map.Strict as Map (Map, empty, fromList, insertLookupWithKey)

data MapList k v = MapList { toMap :: Map k v, toList :: [(k, v)] }
  deriving (Functor)

instance (Eq k, Eq v) => Eq (MapList k v) where
  a == b = toMap a == toMap b

instance Foldable (MapList k) where
  foldMap f = foldMap f . elems

instance Ord k => Traversable (MapList k) where
  traverse f m = (Data.Map.List.fromList . zip (keys m) <$>) . traverse f $ elems m

empty :: MapList k v
empty = MapList Map.empty []

fromList :: Ord k => [(k, v)] -> MapList k v
fromList as = MapList (Map.fromList as) as

elems :: MapList k v -> [v]
elems = map snd . toList

keys :: MapList k v -> [k]
keys = map fst . toList

insertOr :: (Applicative f, Ord k) => k -> v -> (v -> f (MapList k v)) -> MapList k v -> f (MapList k v)
insertOr k v onExisting (MapList m l) =
  case insertLookupWithKey noReplace k v m of
    (Nothing, m') -> pure $ MapList m' (l ++ [(k, v)])
    (Just existing, _) -> onExisting existing
  where noReplace _ _ a = a
