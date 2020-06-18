module BinaryTrie
  ( Trie
  , fromList
  ) where

import qualified Data.Map as Map

data Trie a
  = Empty
  | Node a (Trie a) (Map.Map a (Trie a))
  deriving (Show)

fromList :: (Ord a) => [[a]] -> Trie a
fromList = foldr add Empty

add :: (Ord a) => [a] -> Trie a -> Trie a
add [] t = t
add (x:xs) Empty = Node x (add xs Empty) Map.empty
add (x:xs) (Node y match misses)
  | x == y = Node y (add xs match) misses
  | otherwise =
    let addMiss Nothing = Just (add xs match)
        addMiss (Just t) = Just (add xs t)
     in Node y match (Map.alter addMiss x misses)
