-- | Implementation of a set whose elements are ordered by insert time.
module Reactive.Banana.Type.OSet
  ( OSet,
    empty,
    insert,
    inOrder,
  )
where

import qualified Data.HashMap.Strict as HashMap
import qualified Data.List as List
import Reactive.Banana.Prelude

data OSet a
  = OSet !(HashMap a Int) !Int

empty :: OSet a
empty =
  OSet HashMap.empty 0

-- | Add an element to an ordered set, if it doesn't already exist.
insert :: (Eq a, Hashable a) => OSet a -> a -> OSet a
insert (OSet xs n) x =
  OSet (HashMap.insertWith (\_new old -> old) x n xs) (n + 1)

-- | Reorder a list of elements to appear as they were inserted into the set.
-- Remove any elements from the list that do not appear in the set.
inOrder :: (Eq a, Hashable a) => [(a, b)] -> OSet a -> [(a, b)]
inOrder xs (OSet bag _) =
  map snd $
    List.sortOn fst $
      mapMaybe (\x -> (,x) <$> HashMap.lookup (fst x) bag) xs
