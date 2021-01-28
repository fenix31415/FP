module Block1.Task1Spec where

import           Block1.Task1
import           Test.Hspec

spec :: Spec
spec = do
    it "nextDay" $ do
        nextDay (FromNat 0) `shouldBe` FromNat 1
        nextDay (FromNat 1) `shouldBe` FromNat 2
        nextDay (FromNat 2) `shouldBe` FromNat 3
        nextDay (FromNat 3) `shouldBe` FromNat 4
        nextDay (FromNat 4) `shouldBe` FromNat 5
        nextDay (FromNat 5) `shouldBe` FromNat 6
        nextDay (FromNat 6) `shouldBe` FromNat 0

        (nextDay . nextDay) (FromNat 6) `shouldBe` FromNat 1
        (nextDay . nextDay) (FromNat 0) `shouldBe` FromNat 2
        (nextDay . nextDay) (FromNat 1) `shouldBe` FromNat 3
        (nextDay . nextDay) (FromNat 2) `shouldBe` FromNat 4
        (nextDay . nextDay) (FromNat 3) `shouldBe` FromNat 5
        (nextDay . nextDay) (FromNat 4) `shouldBe` FromNat 6
        (nextDay . nextDay) (FromNat 5) `shouldBe` FromNat 0

        (nextDay . nextDay . nextDay) (FromNat 3) `shouldBe` FromNat 6
        (nextDay . nextDay . nextDay) (FromNat 4) `shouldBe` FromNat 0
        (nextDay . nextDay . nextDay) (FromNat 5) `shouldBe` FromNat 1

        (nextDay . nextDay . nextDay . nextDay) (FromNat 4) `shouldBe` FromNat 1
        (nextDay . nextDay . nextDay . nextDay . nextDay . nextDay . nextDay) (FromNat 4) `shouldBe` FromNat 4

    it "afterDays" $ do
        afterDays (FromNat 0) 0 `shouldBe` FromNat 0
        afterDays (FromNat 1) 0 `shouldBe` FromNat 1
        afterDays (FromNat 6) 0 `shouldBe` FromNat 6

        afterDays (FromNat 6) 2 `shouldBe` FromNat 1
        afterDays (FromNat 5) 2 `shouldBe` FromNat 0
        afterDays (FromNat 4) 2 `shouldBe` FromNat 6
        afterDays (FromNat 0) 2 `shouldBe` FromNat 2

        afterDays (FromNat 6) 6 `shouldBe` FromNat 5
        afterDays (FromNat 0) 6 `shouldBe` FromNat 6
        afterDays (FromNat 1) 6 `shouldBe` FromNat 0
        afterDays (FromNat 2) 6 `shouldBe` FromNat 1

        afterDays (FromNat 0) (7 * 31415 + 0) `shouldBe` FromNat 0
        afterDays (FromNat 1) (7 * 31415 + 1) `shouldBe` FromNat 2
        afterDays (FromNat 2) (7 * 31415 + 2) `shouldBe` FromNat 4
        afterDays (FromNat 3) (7 * 31415 + 3) `shouldBe` FromNat 6
        afterDays (FromNat 4) (7 * 31415 + 4) `shouldBe` FromNat 1
        afterDays (FromNat 5) (7 * 31415 + 5) `shouldBe` FromNat 3
        afterDays (FromNat 6) (7 * 31415 + 6) `shouldBe` FromNat 5

    it "isWeekend" $ do
        isWeekend (FromNat 0) `shouldBe` False
        isWeekend (FromNat 1) `shouldBe` False
        isWeekend (FromNat 2) `shouldBe` False
        isWeekend (FromNat 3) `shouldBe` False
        isWeekend (FromNat 4) `shouldBe` False
        isWeekend (FromNat 5) `shouldBe` True
        isWeekend (FromNat 6) `shouldBe` True

    it "daysToParty" $ do
        daysToParty (FromNat 0) `shouldBe` 4
        daysToParty (FromNat 1) `shouldBe` 3
        daysToParty (FromNat 2) `shouldBe` 2
        daysToParty (FromNat 3) `shouldBe` 1
        daysToParty (FromNat 4) `shouldBe` 0
        daysToParty (FromNat 5) `shouldBe` 6
        daysToParty (FromNat 6) `shouldBe` 5
