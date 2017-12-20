module Day_1 where

-- http://adventofcode.com/2017/day/1

import Data.Char


-- Convert string of digits into list of integers
convert :: String -> [Int]
convert = map $ flip (-) 48 . ord

-- Find sum of all digits that match the n-th next digit (wraps)
findSum :: Int -> [Int] -> Int
findSum n = sum . map match . pairs
  where
    pairs = zip <*> (drop n . cycle)
    match (a, b) | a == b    = a
                 | otherwise = 0


main = do
  solve1 "1122"
  solve1 "1111"
  solve1 "1234"
  solve1 "91212129"
  solve2 "1212"
  solve2 "1221"
  solve2 "123425"
  solve2 "123123"
  solve2 "12131415"

  where
    solve1 x = printSolution (findSum 1) x
    solve2 x = printSolution (findSum $ length x ` div` 2) x
    printSolution f x = putStrLn $ concat [ "'", x, "' => ", show . f . convert $ x ]
