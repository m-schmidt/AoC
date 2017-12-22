module Day_6 where

-- http://adventofcode.com/2017/day/6

import qualified Data.Map.Strict as Map

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vect


-- Convert string of numbers into a vector of integers
convert :: String -> Vector Int
convert = Vect.fromList . map read . words

-- Execute one round of reallocation
realloc :: Vector Int -> Vector Int
realloc v = Vect.imap update v
  where
    update i x | i < maxIdx = x + d + fromEnum (i <= maxIdx + m - vLen)
               | i > maxIdx = x + d + fromEnum (i <= maxIdx + m)
               | otherwise    = d

    vLen   = Vect.length v
    maxIdx = Vect.maxIndex v
    (d, m) = Vect.unsafeIndex v maxIdx `divMod` vLen

-- Count number of allocation steps before cycle starts and its length
allocationCycle :: Vector Int -> (Int, Int)
allocationCycle = go Map.empty . iterate realloc
  where
    go m (v:vs) | v `Map.member` m = (Map.size m, Map.size m - m Map.! v)
                | otherwise        = go (Map.insert v (Map.size m) m) vs
    go _ _ = undefined


main :: IO ()
main = do
  putStrLn $ solve "0 2 7 0"
  where
    solve x = concat [ "number of steps / cycle length = ", show . allocationCycle . convert $ x ]
