module Block1.Task3Spec where

import           Block1.Task3
import           Test.Hspec

sqr :: Int -> Int
sqr a = a * a

maybeDiv2 :: Int -> Maybe Int
maybeDiv2 n
  | even n = Just (n `div` 2)
  | otherwise = Nothing

dup :: Int -> [Int]
dup n = replicate n n

monadCalc :: Int -> NonEmpty Int
monadCalc x = x :| [x+1,x+2]

spec :: Spec
spec = do
  let one = 1 :| [] :: NonEmpty Int
  let sing = 5 :| [] :: NonEmpty Int
  let l1 = 1 :| [2,3,4,5] :: NonEmpty Int
  let l2 = 10 :| [9,8,7] :: NonEmpty Int

  it "fmap" $ do
    fmap (*2) one `shouldBe` 2 :| []
    fmap (+2) sing `shouldBe` 7 :| []
    fmap sqr l1 `shouldBe` 1 :| [4,9,16,25]
    fmap maybeDiv2 l2 `shouldBe` Just 5 :| [Nothing,Just 4,Nothing]

  it "appl" $ do
    pure 1 `shouldBe` one
    pure 5 `shouldBe` sing
    ((+ 1) <$> one) `shouldBe` 2 :| []
    (sqr <$> sing) `shouldBe` 25 :| []
    sqr :| [(+1),(*2)] <*> l1 `shouldBe` 1 :| [4,9,16,25,2,3,4,5,6,2,4,6,8,10]
    maybeDiv2 :| [\x -> Just (10 * x)] <*> l2 `shouldBe` Just 5 :| [Nothing, Just 4, Nothing, Just 100, Just 90, Just 80, Just 70]

  it "fold" $ do
    foldr (+) 0 one `shouldBe` 1
    foldr (+) 0 l1 `shouldBe` 15
    foldr (+) 0 l2 `shouldBe` 34
    foldr min 100 l2 `shouldBe` 7
    foldr max 0 l1 `shouldBe` 5

  it "traverse" $ do
    traverse dup one `shouldBe` [one]
    traverse maybeDiv2 l2 `shouldBe` Nothing
    traverse dup sing `shouldBe` [sing,sing,sing,sing,sing]
    traverse dup l1 `shouldBe` replicate 120 l1
