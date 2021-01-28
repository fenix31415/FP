{-# LANGUAGE InstanceSigs #-}

module Block1.Task3 where

data NonEmpty a = a :| [a] deriving(Show, Eq)

instance Functor NonEmpty where
  fmap :: (a -> b) -> NonEmpty a -> NonEmpty b
  fmap f (x :| xs) = f x :| fmap f xs

instance Applicative NonEmpty where
  pure :: a -> NonEmpty a
  pure x = x :| []

  (<*>) :: NonEmpty (a -> b) -> NonEmpty a -> NonEmpty b
  f :| fs <*> x :| xs = f x :| (fmap f xs ++ (fs <*> x : xs))

instance Foldable NonEmpty where
  foldr :: (a -> b -> b) -> b -> NonEmpty a -> b
  foldr f z (x :| xs) = f x $ foldr f z xs

instance Traversable NonEmpty where
  traverse :: Applicative f => (a -> f b) -> NonEmpty a -> f (NonEmpty b)
  traverse f (x :| xs) = (:|) <$> f x <*> traverse f xs

instance Monad NonEmpty where
  (>>=) :: NonEmpty a -> (a -> NonEmpty b) -> NonEmpty b
  x :| xs >>= f = fx :| (fxs ++ (xs >>= toList . f))
    where toList (a :| as) = a : as
          fx :| fxs = f x
