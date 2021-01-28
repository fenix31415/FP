{-# LANGUAGE BangPatterns #-}
module TestUtils where

import Control.Monad (replicateM)
import Task1 (Point (..), minus, plus)
import Test.Hspec (Expectation, anyException, shouldBe, shouldThrow)
import Test.QuickCheck (Arbitrary (arbitrary), Gen, choose, elements, resize, sized)

p1'::Point
p1' = Point 0 0
p2'::Point
p2' = Point 1 0
p3'::Point
p3' = Point 1 1
p4'::Point
p4' = Point 0 1

square :: [Point]
square = [p1',p2',p3',p4']

triangle :: [Point]
triangle = [p3', p4', p2']

cross :: [Point]
cross = [Point 15 1, Point 10 1, Point 10 2, Point 15 2, Point 15 3, Point 20 3, Point 20 2, Point 25 2, Point 25 1, Point 20 1, Point 20 0, Point 15 0]

crossArea :: Int
crossArea = 50

crossPerim :: Double
crossPerim = 36

---------------------------------------
-- Triangle generators
---------------------------------------

genCoord :: Gen Int
genCoord = choose(-1000, 1000)

genPoint :: Gen Point
genPoint = Point <$> genCoord <*> genCoord

genTriangle :: Gen ([Point], Int)
genTriangle = do
  a <- genCoord
  b <- genCoord
  p <- genPoint
  let px = plus p $ Point a 0
  let py = plus p $ Point 0 b
  let ans = abs $ a * b
  return ([px, p, py], ans)



---------------------------------------
-- Rectangle generators
---------------------------------------

genRectangle :: Gen ([Point], (Double, Int))
genRectangle = do
    p1@(Point x1 y1) <- genPoint
    p3@(Point x3 y3) <- genPoint
    let p2 = Point x3 y1
    let p4 = Point x1 y3
    let (Point w h) = minus p1 p3
    let !ansArea = abs $ 2 * w * h
    let !ansPerimeter = fromIntegral $ 2 * (abs w + abs h) :: Double
    return ([p1,p2,p3,p4], (ansPerimeter, ansArea))


