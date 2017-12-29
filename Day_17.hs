module Day_17 where

-- http://adventofcode.com/2017/day/17

import Data.List


-- |Number of forward steps for each insertion step
forwardSteps :: Int
forwardSteps = 3


-- |List of initial and iteratively updated current position
currentPositions :: [Int]
currentPositions = map snd $ iterate update (1, 0)
  where
    update (w, cur) = (w+1, 1 + (cur+forwardSteps) `mod` w)


-- |Count number of backward-steps until two insertion positions match
countSteps :: Int -> Int -> Int -> Int
countSteps pIns pDst width = go 0 pIns pDst width
  where
    go n i j w | i == j    = n
               | otherwise = go (n+1) i' j' (w-1)
               where
               -- insertion position one step earlier
               i' = (i - 1 - forwardSteps) `mod` (w-1)
               -- updated j one step earlier
               j' = if i < j then j-1 else j


-- |Track last element inserted at position 1
elementAt1 :: [Int] -> Int
elementAt1 curPos =
  snd $ foldl' update (0, 0) curPos
  where
    update :: (Int, Int) -> Int -> (Int, Int)
    update (i, e) pos
      | pos == 1  = (i+1, i)
      | otherwise = (i+1, e)


main :: IO ()
main = do
  let p2017 = currentPositions !! 2017
  putStrLn $ concat [ "2017 is inserted at position ", show p2017, "." ]

  -- Given the most recent insertion position, a target insertion position and
  -- the current width of the buffer, compute preceeding insertion positions
  -- until it matches the target position.
  let count = countSteps p2017 (p2017+1) 2018
  putStrLn $ concat [ "The successor of element 2017 is ", show (2017 - count), "." ]

  -- Observation: 0 is inserted first and remains at position 0 of the buffer
  -- since elements are always inserted to the right of the current element.
  -- => Track elements inserted at position 1
  let element = elementAt1 $ take 50000000 $ currentPositions
  putStrLn $ concat [ "The successor of element 0 is ", show element, "." ]
