module WildTrie
  ( Trie
  , Wildcard
  , fromList
  , findWildcard
  ) where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Set as Set

-- A prefix tree (trie) with a special Wild node, which represents a place where
-- multiple [a]s match down that branch save for the item at the node.
-- This can be thought of as a regex /./
-- A Node is converted to a Wild as soon as a wildcard match is made (see `add`).
-- NOTE: This makes the assumption that there can only be one wildcard match per branch.
data Trie a
  = End
  | Node (Map.Map a (Trie a))
  | Wild [a] (Set.Set a)
  deriving (Show)

-- Represents a Wild node as:
-- (common items before, differing items at wildcard, common items after)
-- aka. (init, options, tail)
type Wildcard a = ([a], [a], [a])

fromList :: (Ord a) => [[a]] -> Trie a
fromList = foldr add End

singleton :: (Ord a) => [a] -> Trie a
singleton [] = End
singleton (x:xs) = Node $ Map.singleton x (singleton xs)

contains :: (Ord a) => [a] -> Trie a -> Bool
contains [] _ = True
contains _ End = False
contains _ (Wild _ _) = False
contains (x:xs) (Node branches) =
  case Map.lookup x branches of
    Nothing -> False
    Just t -> contains xs t

add :: (Ord a) => [a] -> Trie a -> Trie a
add [] t = t
add xs End = singleton xs
-- For wildcard node, add to diffs if tails match
add (x:xs) t@(Wild ys options)
  | xs == ys = Wild ys $ Set.insert x options
  | otherwise = t
add (x:xs) (Node branches)
  -- If we match on the next item in a node, keep traversing
  | Map.member x branches = Node $ Map.adjust (add xs) x branches
  -- If and only if next item doesn't match, we can try turning the node into a wildcard
  | otherwise =
    case tryWildcard (x : xs) branches of
      Nothing -> Node $ Map.insert x (singleton xs) branches
      Just w -> w

-- If we find a branch that matches the tail, having not matched the head, we've found a wildcard
tryWildcard :: (Ord a) => [a] -> Map.Map a (Trie a) -> Maybe (Trie a)
tryWildcard (x:xs) branches = do
  (y, _) <- List.find (contains xs . snd) $ Map.assocs branches
  return $ Wild xs (Set.fromList [x, y])

-- Find the first Wildcard node
findWildcard :: Trie a -> Maybe (Wildcard a)
findWildcard = findWildcard' []
  where
    findWildcard' _ End = Nothing
    findWildcard' init (Wild tail options) =
      Just (init, Set.toList options, tail)
    findWildcard' init (Node branches) =
      listToMaybe $
      mapMaybe (\(x, t) -> findWildcard' (init ++ [x]) t) $ Map.assocs branches
