module Block3.Task2
 ( ok
 , eof
 , satisfy
 , element
 , stream
 ) where

import Block3.Task1

ok :: Parser s ()
ok = Parser $ \s -> Just((), s)

eof :: Parser s ()
eof = Parser $ \s ->
  case s of
    [] -> Just ((), [])
    _ -> Nothing

satisfy :: (s -> Bool) -> Parser s s
satisfy p = Parser $ \s ->
  case s of
    (x:xs) ->
      if p x
      then Just (x, xs)
      else Nothing
    [] -> Nothing

element :: (Eq s) => s -> Parser s s
element c = satisfy (== c)

stream :: Eq s => [s] -> Parser s [s]
stream [] = return []
stream (c:cs) = element c >>= \x -> Parser $ \s ->
  case runParser (stream cs) s of
    Just (ans, s') -> Just (x : ans, s')
    Nothing -> Nothing

