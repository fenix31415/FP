module Utils (removeFromList, updateList, nonAppendEl) where

import Data.List.NonEmpty as NE (NonEmpty (..))

{-|
    Removes all elements from the list, that is equals to given element
    Returns filtered list
-}
removeFromList :: Eq a => [a] -> a -> [a]
removeFromList contents file = filter (file /=) contents

{-|
    Changes an element in the list
    Returns modified list
-}
updateList :: Eq a => [a] -> a -> a -> [a]
updateList contents file newFile = removeFromList contents file ++ [newFile]

{-|
    Appends an element at the end of the 'NonEmpty' container
    Returns new 'NonEmpty' container
-}
nonAppendEl :: NonEmpty a -> a -> NonEmpty a
nonAppendEl ne a = ne <> (a :| [])
