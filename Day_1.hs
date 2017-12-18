module Day_1 where

import Data.Char

{-
  http://adventofcode.com/2017/day/1

  Part 1:

  [...]

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

  Part 2:

  [...]

  Now, instead of considering the next digit, it wants you to
  consider the digit halfway around the circular list. That is,
  if your list contains 10 items, only include a digit in your
  sum if the digit 10/2 = 5 steps forward matches it. Fortunately,
  your list has an even number of elements.

  For example:

  - 1212 produces 6: the list contains 4 items, and all four digits
    match the digit 2 items ahead.

  - 1221 produces 0, because every comparison is between a 1 and
    a 2.

  - 123425 produces 4, because both 2s match each other, but no
    other digit has a match.

  - 123123 produces 12.

  - 12131415 produces 4.
-}


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
