module Block1.Task2Spec where

import           Block1.Task2
import           Test.Hspec

spec :: Spec
spec = do
    it "eq" $ do
        Z == Z `shouldBe` True
        Z /= Z `shouldBe` False
        S Z == Z `shouldBe` False
        S Z /= Z `shouldBe` True
        Z == S Z `shouldBe` False
        Z /= S Z `shouldBe` True
        (S . S . S) Z == Z `shouldBe` False
        (S . S . S) Z /= Z `shouldBe` True
        (S . S . S) Z == (S . S) Z `shouldBe` False
        (S . S . S) Z /= (S . S) Z `shouldBe` True
        (S . S . S) Z == (S . S . S) Z `shouldBe` True
        (S . S . S) Z /= (S . S . S) Z `shouldBe` False
        (S . S . S) Z == (S . S . S . S) Z `shouldBe` False
        (S . S . S) Z /= (S . S . S . S) Z `shouldBe` True

    it "order" $ do
        Z >= Z `shouldBe` True
        Z <= Z `shouldBe` True
        Z < Z `shouldBe` False
        Z > Z `shouldBe` False

        S Z >= Z `shouldBe` True
        S Z > Z `shouldBe` True
        S Z <= Z `shouldBe` False
        S Z < Z `shouldBe` False

        (S . S . S) Z < (S . S . S . S) Z `shouldBe` True
        (S . S . S) Z <= (S . S . S . S) Z `shouldBe` True
        (S . S . S) Z >= (S . S . S . S) Z `shouldBe` False
        (S . S . S) Z > (S . S . S . S) Z `shouldBe` False

    it "+" $ do
        Z +.+ Z `shouldBe` Z
        Z +.+ S Z `shouldBe` S Z
        (S . S) Z +.+ Z `shouldBe` (S . S) Z
        (S . S) Z +.+ S Z `shouldBe` (S . S . S) Z

    it "-" $ do
        Z -.- Z `shouldBe` Z
        Z -.- S Z `shouldBe` Z
        S Z -.- S Z `shouldBe` Z
        (S . S) Z -.- S Z `shouldBe` S Z
        (S . S . S) Z -.- S Z `shouldBe` (S . S) Z

    it "*" $ do
        Z *.* Z `shouldBe` Z
        S Z *.* Z `shouldBe` Z
        S Z *.* S Z `shouldBe` S Z
        (S . S) Z *.* S Z `shouldBe` (S . S) Z
        (S . S . S) Z *.* (S . S) Z `shouldBe` (S . S . S . S . S . S) Z

    it "converts" $ do
        intToNat 0 `shouldBe` Z
        intToNat 1 `shouldBe` S Z
        intToNat 2 `shouldBe` (S . S) Z

        natToInt Z `shouldBe` 0
        natToInt (S Z) `shouldBe` 1
        natToInt (S . S . S . S . S $ Z) `shouldBe` 5

        (natToInt . intToNat $ 10) `shouldBe` 10
        (natToInt . intToNat $ 100) `shouldBe` 100
        (natToInt . intToNat $ 31415) `shouldBe` 31415
