module Block1.Task2 where

data Nat = Z | S Nat
    deriving Show

(+.+) :: Nat -> Nat -> Nat
n +.+ Z = n
Z +.+ n = n
(S n) +.+ (S m) = (S . S) (n +.+ m)

(-.-) :: Nat -> Nat -> Nat
n -.- Z = n
Z -.- _ = Z
(S n) -.- (S m) = n -.- m

(*.*) :: Nat -> Nat -> Nat
Z *.* _ = Z
(S n) *.* m = m +.+ (n *.* m)

intToNat :: Integer -> Nat
intToNat n
  | n == 0 = Z
  | n > 0 = (S . intToNat) (n Prelude.- 1)
  | otherwise = error "arg is negative"

natToInt :: Nat -> Integer
natToInt Z = 0
natToInt (S n) = natToInt n Prelude.+ 1

instance Ord Nat where
  Z <= Z = True
  (S n) <= (S m) = n <= m
  Z <= (S _) = True
  (S _) <= Z = False

instance Eq Nat where
  Z == Z = True
  (S n) == (S m) = n == m
  _ == _ = False
