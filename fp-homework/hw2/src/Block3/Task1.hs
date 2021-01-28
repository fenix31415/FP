{-# LANGUAGE InstanceSigs #-}

module Block3.Task1
 ( Parser (..)
 ) where

import Control.Applicative

newtype Parser s a = Parser { runParser :: [s] -> Maybe (a, [s]) }

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Functor (Parser s) where
  fmap :: (a -> b) -> Parser s a -> Parser s b
  fmap f (Parser a) = Parser $ fmap (first f) . a

instance Applicative (Parser s) where
  pure :: a -> Parser s a
  pure a = Parser $ \s -> Just (a, s)

  (<*>) :: Parser s (a -> b) -> Parser s a -> Parser s b
  Parser f <*> Parser a = Parser $ \s ->
    case f s of
      Just (fun, s') ->
        case a s' of
          Just (x, s'') -> Just (fun x, s'')
          _ -> Nothing
      _ -> Nothing

instance Alternative (Parser s) where
  empty :: Parser s a
  empty = Parser $ const Nothing

  (<|>) :: Parser s a -> Parser s a -> Parser s a
  Parser l <|> Parser r = Parser $ \s ->
    case l s of
      Nothing -> r s
      ans -> ans

instance Monad (Parser s) where
  return :: a -> Parser s a
  return a = Parser $ \s -> Just (a, s)

  (>>=) :: Parser s a -> (a -> Parser s b) -> Parser s b
  Parser a >>= f = Parser $ \s ->
      case a s of
        Just (a', xs) -> runParser (f a') xs
        _ -> Nothing
