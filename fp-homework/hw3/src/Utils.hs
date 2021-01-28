{-# LANGUAGE ScopedTypeVariables #-}

module Utils where

import Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

(<:|) :: NonEmpty a -> a -> NonEmpty a
(<:|) ne a = ne <> (a :| [])

remove :: Eq a => [a] -> a -> [a]
remove contents file = Prelude.filter (file /=) contents

update :: Eq a => [a] -> a -> a -> [a]
update contents file newFile = remove contents file ++ [newFile]

infixl 1 `orElse`
orElse :: Maybe a -> a -> a
orElse = flip fromMaybe

readMaybeInt :: String -> Maybe Int
readMaybeInt = readMaybe
