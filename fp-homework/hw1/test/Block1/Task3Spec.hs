{-# LANGUAGE OverloadedLists #-}

module Block1.Task3Spec where

import           Block1.Task3
import           Test.Hspec

node0 :: Tree Integer
node0 = Node { values = [0], left = Leaf, right = Leaf }
node2 :: Tree Integer
node2 = Node { values = [2], left = Leaf, right = Leaf }
node4 :: Tree Integer
node4 = Node { values = [4], left = Leaf, right = Leaf }

node1 :: Tree Integer
node1 = Node { values = [1], left = node0, right = node2 }
node3 :: Tree Integer
node3 = Node { values = [3], left = node1, right = node4 }

spec :: Spec
spec = do
      it "size" $ do
              size Leaf `shouldBe` 0
              size node0 `shouldBe` 1
              size node1 `shouldBe` 3
              size node2 `shouldBe` 1
              size node3 `shouldBe` 5
              size node4 `shouldBe` 1

      it "find" $ do
              find Leaf (1 :: Int) `shouldBe` Nothing
              find node3 1 `shouldBe` Just [1]
              find node1 2 `shouldBe` Just [2]

      it "insert" $ do
              find (insertTree 0 node3) 0 `shouldBe` Just [0, 0]
              find (insertTree 5 node3) 5 `shouldBe` Just [5]
