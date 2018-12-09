module Day_15 where

-- http://adventofcode.com/2017/day/15

import Data.Bits


generator :: Int -> Int -> [Int]
generator v_init factor = drop 1 $ iterate gen v_init
  where
    gen v_prev = (v_prev * factor) `mod` 2147483647

lowbits :: [Int] -> [Int] -> [Int]
lowbits = zipWith mask
  where
    mask a b = (a `xor` b) .&. 65535

main :: IO ()
main = do
  let gen_a   = generator 65 16807
  let gen_b   = generator 8921 48271
  let samples = take 40000000 $ lowbits gen_a gen_b
  putStrLn $ show $ length $ filter (==0) samples

  let gen_a'   = filter_modulus 4 gen_a
  let gen_b'   = filter_modulus 8 gen_b
  let samples' = take 5000000 $ lowbits gen_a' gen_b'
  putStrLn $ show $ length $ filter (==0) samples'

  where
    filter_modulus m = filter (\x -> x `mod` m == 0)
