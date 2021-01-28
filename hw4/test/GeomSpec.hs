module GeomSpec where

import Control.Arrow ((&&&))
import Task1 (doubleArea, perimeter)
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Testable (property), forAll)
import TestUtils (cross, crossArea, crossPerim, genRectangle, genTriangle, square, triangle)

spec :: Spec
spec = do
  it "qquad & triangle" $ do
    doubleArea [] `shouldBe` 0
    perimeter [] `shouldBe` 0
    doubleArea triangle `shouldBe` 1
    doubleArea square `shouldBe` 2
    perimeter square `shouldBe` 4
    perimeter cross `shouldBe` crossPerim
    doubleArea cross `shouldBe` crossArea

  it "trianrle area" $ property $ forAll genTriangle $ \(tr, area) ->
      doubleArea tr `shouldBe` area

  it "rec area&per" $ property $ forAll genRectangle $ \(rect, ans) ->
      (perimeter &&& doubleArea) rect `shouldBe` ans
