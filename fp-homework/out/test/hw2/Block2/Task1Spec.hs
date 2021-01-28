module Block2.Task1Spec where

import           Block2.Task1
import           Test.Hspec

spec :: Spec
spec = do
  let expr1 = Add (ConstNumber 2) (ConstNumber 2) :: Expr Int
  let expr2 = Add (ConstNumber 2) (Mul (ConstNumber 2) (ConstNumber 2)) :: Expr Int
  let expr3 = Sub (Add (Pow (ConstNumber 3) (ConstNumber 2)) (Mul (ConstNumber 4) (ConstNumber 4))) (Mul (ConstNumber 5) (ConstNumber 5)) :: Expr Int

  it "simple" $ do
    eval expr1 `shouldBe` Right 4
    eval expr2 `shouldBe` Right 6
    eval expr3 `shouldBe` Right 0

  it "pow mul div" $ do
    eval (Div (Add expr2 expr1) (Add expr3 expr1)) `shouldBe` Right 2
    eval (Pow (Add expr2 expr1) (Mul expr3 expr1)) `shouldBe` Right 1
    eval (Pow (Sub expr1 expr2) expr3) `shouldBe` Right 1
    eval (Pow (Sub expr1 expr2) expr1) `shouldBe` Right 16
    eval (Mul (Sub (Div expr2 expr1) (Div (Sub expr2 expr1) (ConstNumber 2 :: Expr Int))) (Sub expr2 (Add expr3 expr1))) `shouldBe` Right 0

  it "errors" $ do
    eval (Div (Sub expr2 expr1) expr3) `shouldBe` Left DivBy0
    eval (Div (Mul expr3 expr1) expr3) `shouldBe` Left DivBy0
    eval (Pow (Mul expr3 expr1) (Sub expr1 expr2)) `shouldBe` Left NegPow
    eval (Pow (Mul expr1 expr1) (Sub expr1 expr2)) `shouldBe` Left NegPow
