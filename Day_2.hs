module Day_2 where

import Control.Applicative

{-
  http://adventofcode.com/2017/day/2

  As you walk through the door, a glowing humanoid shape yells in
  your direction. "You there! Your state appears to be idle. Come
  help us repair the corruption in this spreadsheet - if we take
  another millisecond, we'll have to display an hourglass cursor!"

  The spreadsheet consists of rows of apparently-random numbers.
  To make sure the recovery process is on the right track, they
  need you to calculate the spreadsheet's checksum. For each row,
  determine the difference between the largest value and the
  smallest value; the checksum is the sum of all of these
  differences.

  For example, given the following spreadsheet:

    5 1 9 5
    7 5 3
    2 4 6 8

  - The first row's largest and smallest values are 9 and 1, and
    their difference is 8.

  - The second row's largest and smallest values are 7 and 3, and
    their difference is 4.

  - The third row's difference is 6.

  In this example, the spreadsheet's checksum would be:

    8 + 4 + 6 = 18.
-}


-- Parse a string representation of an input matrix
convert :: String -> [[Int]]
convert = map (map read . words) . filter (not . null) . lines

-- Sums the differences of all rows of a matrix
checkSum :: [[Int]] -> Int
checkSum = sum . map diff
  where
    diff = liftA2 (-) maximum minimum


main = do
  putStrLn $ solve "5 1 9 5\n\
                   \7 5 3\n\
                   \2 4 6 8"
  where
    solve x = concat [ x, " => ", show . checkSum . convert $ x ]
