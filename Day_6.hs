module Day_6 where

-- http://adventofcode.com/2017/day/6

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vect


-- Convert string of numbers into a vector of integers
convert :: String -> Vector Int
convert = Vect.fromList . map read . words

-- Execute one round of reallocation
realloc :: Vector Int -> Vector Int
realloc v = Vect.imap update v
  where
    update i x | i < maxIndex = x + d + fromEnum (i <= maxIndex + m - vLength)
               | i > maxIndex = x + d + fromEnum (i <= maxIndex + m)
               | otherwise    = d

    vLength  = Vect.length v
    maxIndex = Vect.maxIndex v
    (d, m)   = Vect.unsafeIndex v maxIndex `divMod` vLength

-- Count number of allocation steps until cycle starts and its length
allocationCycle :: Vector Int -> (Int, Int)
allocationCycle = go Map.empty . iterate realloc
  where
    go m (v:vs) | v `Map.member` m = (Map.size m, Map.size m - m Map.! v)
                | otherwise        = go (Map.insert v (Map.size m) m) vs


main = do
  putStrLn $ solve "0 2 7 0"
  where
    solve x = concat [ "number of steps / cycle length = ", show . allocCycle . convert $ x ]
