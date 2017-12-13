module Day_1 where

import Data.Char

{-
  http://adventofcode.com/2017/day/1

  You're standing in a room with "digitization quarantine" written
  in LEDs along one wall. The only door is locked, but it includes a
  small interface.

      "Restricted Area - Strictly No Digitized Users Allowed."

  The captcha requires you to review a sequence of digits (your
  puzzle input) and find the sum of all digits that match the next
  digit in the list. The list is circular, so the digit after the
  last digit is the first digit in the list.

  For example:

  - 1122 produces a sum of 3 (1 + 2) because the first digit (1)
    matches the second digit and the third digit (2) matches the
    fourth digit

  - 1111 produces 4 because each digit (all 1) matches the next

  - 1234 produces 0 because no digit matches the next

  - 91212129 produces 9 because the only digit that matches the
    next one is the last digit, 9
-}


-- Convert string of digits into list of integers
convert :: String -> [Int]
convert = map $ flip (-) 48 . ord

-- Find sum of all digits that match the next digit (wraps)
findSum :: [Int] -> Int
findSum = sum . map match . pairs
  where
    pairs = zip <*> (drop 1 . cycle)
    match (a, b) | a == b    = a
                 | otherwise = 0


main = do
  putStrLn $ solve "1122"
  putStrLn $ solve "1111"
  putStrLn $ solve "1234"
  putStrLn $ solve "91212129"
  where
    solve x = concat [ "'", x, "' => ", show . findSum . convert $ x ]
