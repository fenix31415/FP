{-# LANGUAGE InstanceSigs #-}

module Block1.Task2 where

data Tree a = Branch (Tree a) (Tree a) | Leaf a
  deriving(Show)

instance Functor Tree where
  fmap :: (a -> b) -> Tree a -> Tree b
  fmap f (Leaf a) = Leaf $ f a
  fmap f (Branch l r) = Branch (fmap f l) (fmap f r)

instance Applicative Tree where
  pure :: a -> Tree a
  pure = Leaf

  (<*>) :: Tree (a -> b) -> Tree a -> Tree b
  (<*>) (Leaf f) tree = fmap f tree
  Branch fl fr <*> leaf@(Leaf _) = Branch (fl <*> leaf) (fr <*> leaf)
  Branch fl fr <*> t = Branch (fl <*> t) (fr <*> t)

instance Foldable Tree where
  foldr :: (a -> b -> b) -> b -> Tree a -> b
  foldr f z (Leaf a) = f a z
  foldr f z (Branch l r) = foldr f (foldr f z r) l

instance Traversable Tree where
  traverse :: (Applicative f) => (a -> f b) -> Tree a -> f (Tree b)
  traverse f (Leaf a) = Leaf <$> f a
  traverse f (Branch l r) = Branch <$> traverse f l <*> traverse f r

