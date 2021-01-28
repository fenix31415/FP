{-# LANGUAGE OverloadedLists #-}

module Block1.Task3 where

import Data.List.NonEmpty

data Tree t = Leaf | Node
      { values :: NonEmpty t
      , left   :: Tree t
      , right  :: Tree t
      }
  deriving (Show)

isEmpty :: Tree t -> Bool
isEmpty Leaf = True
isEmpty _    = False

size :: Tree t -> Int
size Leaf = 0
size (Node _ l r) = 1 + size l + size r

find :: (Ord t) => Tree t -> t -> Maybe (NonEmpty t)
find Leaf _ = Nothing
find (Node vals l r) x
  | x < val = find l x
  | x > val = find r x
  | otherwise = Just vals
  where
    val = Data.List.NonEmpty.head vals

insertTree :: (Ord t) => t -> Tree t -> Tree t
insertTree x Leaf = Node [x] Leaf Leaf
insertTree x node@(Node vals l r)
  | x < val = node { left = insertTree x l}
  | x > val = node { right = insertTree x r}
  | otherwise = node { values = x <| vals }
  where
    val = Data.List.NonEmpty.head vals

fromList :: (Ord a) => [a] -> Tree a
fromList = foldr insertTree Leaf
