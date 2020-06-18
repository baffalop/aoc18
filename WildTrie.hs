module WildTrie
  ( Trie
  , fromList
  , findWildcard
  ) where

import qualified Data.List as List
import qualified Data.Map as Map
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Set as Set

data Trie a
  = End
  | Node (Map.Map a (Trie a))
  | Wildcard [a] (Set.Set a)
  deriving (Show)

fromList :: (Ord a) => [[a]] -> Trie a
fromList = foldr add End

singleton :: (Ord a) => [a] -> Trie a
singleton [] = End
singleton (x:xs) = Node $ Map.singleton x (singleton xs)

contains :: (Ord a) => [a] -> Trie a -> Bool
contains [] _ = True
contains _ End = False
contains _ (Wildcard _ _) = False
contains (x:xs) (Node branches) =
  case Map.lookup x branches of
    Nothing -> False
    Just t -> contains xs t

add :: (Ord a) => [a] -> Trie a -> Trie a
add [] t = t
add xs End = singleton xs
-- For wildcard node, add to diffs if tails match
add (x:xs) t@(Wildcard ys options)
  | xs == ys = Wildcard ys $ Set.insert x options
  | otherwise = t
add (x:xs) (Node branches)
  | Map.member x branches = Node $ Map.adjust (add xs) x branches
  | otherwise =
    case tryWildcard (x : xs) branches of
      Nothing -> Node $ Map.insert x (singleton xs) branches
      Just w -> w

tryWildcard :: (Ord a) => [a] -> Map.Map a (Trie a) -> Maybe (Trie a)
tryWildcard (x:xs) branches = do
  (y, _) <- List.find (contains xs . snd) $ Map.assocs branches
  return $ Wildcard xs (Set.fromList [x, y])

findWildcard :: Trie a -> Maybe ([a], [a], [a])
findWildcard = findWildcard' []
  where
    findWildcard' _ End = Nothing
    findWildcard' init (Wildcard tail options) =
      Just (init, Set.toList options, tail)
    findWildcard' init (Node branches) =
      listToMaybe $
      mapMaybe (\(x, t) -> findWildcard' (init ++ [x]) t) $ Map.assocs branches
