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

-- |Given the current width `w` of the buffer and the most recent
-- insertion position `pIns` compute how many steps earlier the
-- element at position `pDst` in the buffer was inserted
go :: Int -> Int -> Int -> Int -> Int
go n w pIns pDst
  | pIns == pDst = n
  | otherwise    = go (n+1) (w-1) pIns' pDst'
    where
      -- insertion position one step earlier
      pIns' = (pIns - 1 - forwardSteps) `mod` (w-1)
      -- updated pDst one step earlier
      pDst' = if pIns < pDst then pDst-1 else pDst


main :: IO ()
main = do
  let p2017 = currentPositions !! 2017
  putStrLn $ concat [ "2017 was inserted at position ", show p2017, "." ]

  let count = go 0 2018 p2017 (p2017+1)
  putStrLn $ concat [ "The successor of 2017 is ", show (2017 - count), "." ]
