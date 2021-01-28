module Block2.Task1 where

data ArithmeticError = DivBy0 | NegPow deriving (Show, Eq)

type Result a = Either ArithmeticError a

data Expr a = ConstNumber a
  | Pow (Expr a) (Expr a)
  | Mul (Expr a) (Expr a)
  | Div (Expr a) (Expr a)
  | Add (Expr a) (Expr a)
  | Sub (Expr a) (Expr a)
  deriving (Show, Eq)

compute :: (Integral a) => (a -> a -> a) -> Expr a -> Expr a -> Result a
compute f x y = Right f <*> eval x <*> eval y

computeSafe :: (Integral a) => (a -> a -> Result a) -> Expr a -> Expr a -> Result a
computeSafe f x y = do
  xEval <- eval x
  yEval <- eval y
  f xEval yEval

eval :: (Integral a) => Expr a -> Result a
eval (ConstNumber x) = Right x
eval (Add a b) = compute (+) a b
eval (Sub a b) = compute (-) a b
eval (Mul a b) = compute (*) a b
eval (Div a b) = computeSafe safeDiv a b
  where
    safeDiv x y
      | y == 0 = Left DivBy0
      | otherwise = Right $ div x y
eval (Pow a b) = computeSafe savePow a b
  where
    savePow x y
      | y < 0 = Left NegPow
      | otherwise = Right $ x ^ y

