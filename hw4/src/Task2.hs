{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE Strict       #-}
module Task2 where

import Control.DeepSeq (deepseq)
import Control.Parallel.Strategies (parMap, rpar)
import Data.List (foldl', unfoldr)
import System.Random as R (Random (randomRs), mkStdGen)

type PartialComp = (Int, Int)
type Point = (Double,Double)

randomlist_ :: Int -> Int -> [Double]
randomlist_ seed n = take n $ randomRs (0, 1.0) (mkStdGen seed)

randomPoints :: Int -> Int -> [Point]
randomPoints seed n = zip (randomlist_ seed n) (randomlist_ (seed + 1) n)

accum_ :: PartialComp -> Bool -> PartialComp
accum_ (!s, !t) True  = (s+1, t+1)
accum_ (!s, !t) False = (s, t+1)

accum :: PartialComp -> PartialComp -> PartialComp
accum (!s, !t) (!s', !t') = (s+s', t+t')

computeS_ :: [Point] -> (Point -> Bool) -> PartialComp
computeS_ inp f = foldl' accum_ (0, 0) (map f inp)

computeS :: Int -> Int -> PartialComp
computeS seed n = computeS_ (randomPoints seed n) isBelow

computeP :: Int -> Int -> PartialComp
computeP n c =
    let n' = n `div` c
        f seed = computeS seed c
        !myes = parMap rpar f (take n' [1..])
    in deepseq myes $ foldl' accum (0, 0) myes

isBelow :: Point -> Bool
isBelow (x, y) = f x >= y

f :: Double -> Double
f x = -log (cos  $ sin x) * tan x ^ 2

getQuot :: PartialComp -> Double
getQuot (s, t) = fromIntegral s / fromIntegral t

iters :: Int
iters = 10000000

resP :: PartialComp
resP = computeP iters (iters `div` 10)

resS :: PartialComp
resS = computeS 31415 iters :: PartialComp

ansS :: Double
ansS = 2 * getQuot resS

ansP :: Double
ansP = 2 * getQuot resP
