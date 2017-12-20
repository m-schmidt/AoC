module Day_2 where

-- http://adventofcode.com/2017/day/2

import Control.Applicative


-- Parse a string representation of an input matrix
convert :: String -> [[Int]]
convert = map (map read . words) . filter (not . null) . lines

-- Sums the differences of all rows of a matrix
checkSum1 :: [[Int]] -> Int
checkSum1 = sum . map diff
  where
    diff = liftA2 (-) maximum minimum

-- Sums the quotients of the evenly dividable numbers
checkSum2 :: [[Int]] -> Int
checkSum2 = sum . map even_div
  where
    even_div xs = head [ x `div` y | x <- xs, y <- xs, x /= y, x `mod` y == 0]


main = do
  solve checkSum1 "5 1 9 5\n\
                  \7 5 3\n\
                  \2 4 6 8"

  solve checkSum2 "5 9 2 8\n\
                  \9 4 7 3\n\
                  \3 8 6 5"
  where
    solve f x = putStrLn $ concat [ x, " => ", show . f . convert $ x ]
