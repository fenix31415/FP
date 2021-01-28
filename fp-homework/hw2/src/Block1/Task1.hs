module Block1.Task1 where

import Text.Read (readMaybe)

stringSum :: String -> Maybe Int
stringSum s = fmap sum $ traverse readMaybe $ words s
