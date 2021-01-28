{-# LANGUAGE BangPatterns #-}
module Task1 (perimeter, doubleArea, Point(Point), plus, minus) where

import Control.DeepSeq (NFData, rnf)

data Point = Point !Int !Int deriving (Show)

pointOp :: (Int -> Int -> Int) -> Point -> Point -> Point
pointOp op (Point x1 y1) (Point x2 y2) = Point (op x1 x2) (op y1 y2)

plus :: Point -> Point -> Point
plus = pointOp (+)

minus :: Point -> Point -> Point
minus = pointOp (-)

scalarProduct :: Point -> Point -> Int
scalarProduct (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

crossProduct :: Point -> Point -> Int
crossProduct (Point x1 y1) (Point x2 y2) = x1 * y2 - x2 * y1

distance :: Point -> Point -> Double
distance a b = sqrt $ fromIntegral $ scalarProduct c c
  where
    c = minus a b

summ :: Num a => (Point -> Point -> a) -> a -> Point -> [Point] -> a
summ f !acc init [p]        = acc + f p init
summ f !acc init (p1:p2:ps) = summ f (acc + f p1 p2) init (p2:ps)

summInit :: Num a => (Point -> Point -> a) -> [Point] -> a
summInit _ []        = 0
summInit f pts@(p:_) = summ f 0 p pts

perimeter :: [Point] -> Double
perimeter = summInit distance

doubleArea :: [Point] -> Int
doubleArea = abs . summInit crossProduct
