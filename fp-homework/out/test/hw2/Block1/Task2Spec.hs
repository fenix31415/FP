module Block1.Task2Spec where

import           Block1.Task2
import           Test.Hspec

sqr :: Int -> Int
sqr a = a * a

maybeDiv2 :: Int -> Maybe Int
maybeDiv2 n
  | even n = Just (n `div` 2)
  | otherwise = Nothing

dup :: Int -> [Int]
dup n = replicate n n

spec :: Spec
spec = do
  let simpleTree = Branch (Leaf 1) (Leaf 2) :: Tree Int
  let tree123 = Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3) :: Tree Int
  let rightBamboo = Branch (Leaf 4) (Branch (Leaf 3) (Branch (Leaf 2) (Branch (Leaf 1) (Leaf 0)))) :: Tree Int

  it "fmap simpleTree" $ do
    (show $ fmap (+1) simpleTree)      `shouldBe` "Branch (Leaf 2) (Leaf 3)"
    (show $ fmap sqr simpleTree)       `shouldBe` "Branch (Leaf 1) (Leaf 4)"
    (show $ fmap maybeDiv2 simpleTree) `shouldBe` "Branch (Leaf Nothing) (Leaf (Just 1))"
    (show $ fmap dup simpleTree)       `shouldBe` "Branch (Leaf [1]) (Leaf [2,2])"

  it "fmap tree123" $ do
    (show $ fmap (+1) tree123)      `shouldBe` "Branch (Branch (Leaf 2) (Leaf 3)) (Leaf 4)"
    (show $ fmap sqr tree123)       `shouldBe` "Branch (Branch (Leaf 1) (Leaf 4)) (Leaf 9)"
    (show $ fmap maybeDiv2 tree123) `shouldBe` "Branch (Branch (Leaf Nothing) (Leaf (Just 1))) (Leaf Nothing)"
    (show $ fmap dup tree123)       `shouldBe` "Branch (Branch (Leaf [1]) (Leaf [2,2])) (Leaf [3,3,3])"

  it "fmap rightBamboo" $ do
    (show $ fmap (+1) rightBamboo)      `shouldBe` "Branch (Leaf 5) (Branch (Leaf 4) (Branch (Leaf 3) (Branch (Leaf 2) (Leaf 1))))"
    (show $ fmap sqr rightBamboo)       `shouldBe` "Branch (Leaf 16) (Branch (Leaf 9) (Branch (Leaf 4) (Branch (Leaf 1) (Leaf 0))))"
    (show $ fmap maybeDiv2 rightBamboo) `shouldBe` "Branch (Leaf (Just 2)) (Branch (Leaf Nothing) (Branch (Leaf (Just 1)) (Branch (Leaf Nothing) (Leaf (Just 0)))))"
    (show $ fmap dup rightBamboo)       `shouldBe` "Branch (Leaf [4,4,4,4]) (Branch (Leaf [3,3,3]) (Branch (Leaf [2,2]) (Branch (Leaf [1]) (Leaf []))))"

  it "<*> laws id" $ do
    (show $ pure id <*> simpleTree)  `shouldBe` (show $ simpleTree)
    (show $ pure id <*> tree123)     `shouldBe` (show $ tree123)
    (show $ pure id <*> rightBamboo) `shouldBe` (show $ rightBamboo)

  it "<*> laws assoc" $ do
    let assocU = Leaf (+1) :: Tree (Int -> Int)
    let assocV = Leaf (*2) :: Tree (Int -> Int)
    (show $ pure (.) <*> assocU <*> assocV <*> simpleTree)  `shouldBe` (show $ assocU <*> (assocV <*> simpleTree))
    (show $ pure (.) <*> assocU <*> assocV <*> tree123)     `shouldBe` (show $ assocU <*> (assocV <*> tree123))
    (show $ pure (.) <*> assocU <*> assocV <*> rightBamboo) `shouldBe` (show $ assocU <*> (assocV <*> rightBamboo))

    let assocU1 = Branch (Leaf (*2)) (Leaf (+1)) :: Tree (Int -> Int)
    let assocV1 = Branch (Leaf sqr) (Leaf (subtract 4)) :: Tree (Int -> Int)
    (show $ pure (.) <*> assocU1 <*> assocV1 <*> simpleTree)  `shouldBe` (show $ assocU1 <*> (assocV1 <*> simpleTree))
    (show $ pure (.) <*> assocU1 <*> assocV1 <*> tree123)     `shouldBe` (show $ assocU1 <*> (assocV1 <*> tree123))
    (show $ pure (.) <*> assocU1 <*> assocV1 <*> rightBamboo) `shouldBe` (show $ assocU1 <*> (assocV1 <*> rightBamboo))

  it "<*> laws trans" $ do
    let transU = Leaf sqr
    let transU1 = Branch (Leaf sqr) (Leaf (*2))
    let yInt = 2 :: Int
    let transUTree = Branch (Leaf (fmap sqr)) (Leaf (fmap (*2)))
    (show $ transU <*> pure yInt)  `shouldBe` (show $ pure ($ yInt) <*> transU)
    (show $ transU1 <*> pure yInt) `shouldBe` (show $ pure ($ yInt) <*> transU1)
    (show $ transUTree <*> pure simpleTree)  `shouldBe` (show $ pure ($ simpleTree) <*> transUTree)
    (show $ transUTree <*> pure tree123)     `shouldBe` (show $ pure ($ tree123) <*> transUTree)
    (show $ transUTree <*> pure rightBamboo) `shouldBe` (show $ pure ($ rightBamboo) <*> transUTree)

  it "fold" $ do
    foldr (+) 0 simpleTree `shouldBe` 3
    foldr (+) 0 tree123 `shouldBe` 6
    foldr (+) 0 rightBamboo `shouldBe` 10
    foldr max 0 simpleTree `shouldBe` 2
    foldr max 0 tree123 `shouldBe` 3
    foldr max 0 rightBamboo `shouldBe` 4

  it "traverse" $ do
    (show $ traverse dup simpleTree) `shouldBe` "[Branch (Leaf 1) (Leaf 2),Branch (Leaf 1) (Leaf 2)]"
    (show $ traverse maybeDiv2 simpleTree) `shouldBe` "Nothing"
    (show $ traverse maybeDiv2 (fmap (*2) tree123)) `shouldBe` "Just (Branch (Branch (Leaf 1) (Leaf 2)) (Leaf 3))"
    (show $ traverse maybeDiv2 tree123) `shouldBe` "Nothing"
