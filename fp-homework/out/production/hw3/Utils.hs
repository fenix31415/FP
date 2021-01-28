{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

-- | Split list of values by given value.
-- Return non-empty list of lists of stored parts.
splitOn :: forall a . Eq a => a -> [a] -> NonEmpty [a]
splitOn splitElement list =
  case splitResult of
    Just result -> result
    Nothing -> [] :| []
  where
    splitResult :: Maybe (NonEmpty [a])
    splitResult = nonEmpty $ (buffer : splitted)

    (splitted, buffer) = foldr foldFunc ([], []) list

    foldFunc :: a -> ([[a]], [a]) -> ([[a]], [a])
    foldFunc x acc
      | x == splitElement = ((snd acc : fst acc), [])
      | otherwise = (fst acc, x : snd acc)

(<:|) :: NonEmpty a -> a -> NonEmpty a
(<:|) ne a = ne <> (a :| [])

remove :: Eq a => [a] -> a -> [a]
remove contents file = Prelude.filter ((/=) file) contents

update :: Eq a => [a] -> a -> a -> [a]
update contents file newFile = remove contents file ++ [newFile]

infixl 1 `orElse`
orElse :: Maybe a -> a -> a
orElse = flip fromMaybe

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe
