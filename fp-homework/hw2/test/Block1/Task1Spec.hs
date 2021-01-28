module Block1.Task1Spec where

import           Block1.Task1
import           Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

dice :: Gen Int
dice = choose (-100, 100)

spec :: Spec
spec = do
    it "single" $ do
        stringSum "" `shouldBe` Just 0
        stringSum "1" `shouldBe` Just 1
        stringSum "1000" `shouldBe` Just 1000
        stringSum "-100" `shouldBe` Just (-100)
        stringSum "1" `shouldBe` Just 1
    it "a+b" $ do
        stringSum "1 1" `shouldBe` Just 2
        stringSum "10 -10" `shouldBe` Just 0
        stringSum "0 1000" `shouldBe` Just 1000
    it "random tests" $ do
        stringSum "\t 1   10  100     1000" `shouldBe` Just 1111
        stringSum " 1 2 3       4 5 6 7 8  9 10 11" `shouldBe` Just 66
        stringSum "1 1 2 3 5      8 13 21" `shouldBe` Just 54
        stringSum " 0 1 -2 3 -4      5 -6 7 -8 9 -10 11" `shouldBe` Just 6
        stringSum " 1  1  1   1  \t 1   1   1   1         1  1 1 1  1 1  1  1  1   1   1   1" `shouldBe` Just 20
        stringSum "  100000            10000000000                                   1" `shouldBe` Just 10000100001
    it "errors" $ do
        stringSum "q" `shouldBe` Nothing
        stringSum " 1 45 qq" `shouldBe` Nothing
        stringSum " 1 45 qq" `shouldBe` Nothing
        stringSum " 1    2 _    _" `shouldBe` Nothing

    describe "super property tests" $ do
        prop "single" $ \a -> stringSum (show a) `shouldBe` Just a
        prop "_" $ \a -> stringSum (show a) `shouldBe` Just a

    quickCheck $ \x -> x `shouldBe` x

