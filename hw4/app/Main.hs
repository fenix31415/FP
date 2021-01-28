module Main where

import Criterion.Main (Benchmark, bench, bgroup, defaultMain, nf)
import GHC.Float.RealFracMethods (double2Int, floorDoubleInt)
import Task1 as N (Point (..), doubleArea, perimeter)
import Task1Bad as B (Point (..), doubleArea, perimeter)
import Task2 (getQuot, resP, resS)

r :: Double
r = 100000.0

figureFabricNorm :: [N.Point]
figureFabricNorm = [ N.Point
    (floorDoubleInt $ r * cos (fromIntegral t * 2 * 3.1415926 / r :: Double))
    (floorDoubleInt $ r * sin (fromIntegral t * 2 * 3.1415926 / r)) | t <- [0..(double2Int r)]]

figureFabricBad :: [B.Point]
figureFabricBad = [ B.Point
    (floorDoubleInt $ r * cos (fromIntegral t * 2 * 3.1415926 / r :: Double))
    (floorDoubleInt $ r * sin (fromIntegral t * 2 * 3.1415926 / r)) | t <- [0..(double2Int r)]]

benchPer :: Benchmark
benchPer = bgroup "Perimeter" [bench "norm" $ nf N.perimeter figureFabricNorm, bench "bad" $ nf B.perimeter figureFabricBad]

benchArea :: Benchmark
benchArea = bgroup "DoubleArea" [bench "norm" $ nf N.doubleArea figureFabricNorm, bench "bad" $ nf B.doubleArea figureFabricBad]

benchMC :: Benchmark
benchMC = bgroup "MC" [bench "single" $ nf getQuot resS, bench "multi" $ nf getQuot resP]

-- Our benchmark harness.
main :: IO ()
-- main = defaultMain [benchMC]
main = defaultMain [benchArea, benchPer, benchMC]

