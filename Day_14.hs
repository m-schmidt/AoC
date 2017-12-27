module Day_14 where

-- http://adventofcode.com/2017/day/14

import qualified Day_10 as D10
import Text.Printf


-- Count number of bits set in a hash
toBitcount :: [Int] -> Int
toBitcount = sum . map count
  where
    count = length . filter (=='1') . printf "%b"


-- Count used cells in grid defined by a key
countUsed :: String -> Int
countUsed key = sum $ map knothash [0..127]
  where
    knothash :: Int -> Int
    knothash n = toBitcount . D10.densify $ D10.hash (hashfunc n) [0..255]

    hashfunc :: Int -> Int -> Int
    hashfunc n = D10.hashFunc' (printf "%s-%d" key n) 64


main :: IO ()
main = do
  putStrLn $ show $ countUsed "flqrgnkx"
