module Day_5 where

-- http://adventofcode.com/2017/day/5

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap


-- |Convert string of numbers into a map of integers (key is index of jump offset)
convert :: String -> IntMap Int
convert = IntMap.fromList . zip [0..] . map read . words

-- |Execute steps with environment `env` until index is out of bounds using different update functions.
steps :: IntMap Int -> (Int, Int)
steps env = (go update1 0 0 env, go update2 0 0 env)
  where
    go update acc n e = case IntMap.lookup n e of
      Just o -> go update (acc+1) (n+o) (IntMap.adjust update n e)
      _      -> acc

    update1 = (+1)
    update2 n | n >= 3    = n-1
              | otherwise = n+1


main :: IO ()
main = do
  putStrLn $ solve "0 3 0 1 -3"
  where
    solve x = concat [ "number of steps = ", show . steps . convert $ x ]
