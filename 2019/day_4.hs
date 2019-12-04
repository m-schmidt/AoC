-- https://adventofcode.com/2019/day/4

import Control.Monad
import Data.List


-- convert a list of decimal digits into an integer value
value = foldl (\acc d -> acc*10 + d) 0

-- check whether a list contains any consecutive sequence whose length satisfies predicate `p`
has_seq p = any p . fmap length . group 

-- compute all valid passwords whose digits satisfiy predicate `pred`
passwords pred = do
  -- monotonic increasing digits
  d0 <- [0..9]
  d1 <- [d0..9]
  d2 <- [d1..9]
  d3 <- [d2..9]
  d4 <- [d3..9]
  d5 <- [d4..9]
  -- represented value must be in range
  let ds = [d0,d1,d2,d3,d4,d5]
  let v = value ds
  unless (134564 <= v && v <= 585159) mempty  
  -- represented digits must fulfill a predicate
  unless (pred ds) mempty
  -- v is valid
  return v

main :: IO ()
main = do
  print $ length $ passwords (has_seq (> 1))
  print $ length $ passwords (has_seq (==2))
