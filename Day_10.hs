module Day_10 where

-- http://adventofcode.com/2017/day/10

import Data.Bits
import Data.Char
import Data.List
import Text.Printf


-- Length of the circle
circleLength :: Int
circleLength = 256


data Idx = Idx { current :: Int
               , skip    :: Int
               , value   :: Int
               }


-- Compute an index update function to handle a single knot of length `len`
twist :: Int -> Idx -> Idx
twist len (Idx c s x)
    | x+circleLength < c+len = Idx c' s' $ rev (x + circleLength)
    | c <= x && x < c+len    = Idx c' s' $ rev x
    | otherwise              = Idx c' s' x
    where
      rev idx = (c + len - 1 - idx + c) `mod` circleLength
      c'      = (c + len + s) `mod` circleLength
      s'      = s + 1


-- Build hash function doing n rounds of twists defined by a list knot lengths
hashFunc :: [Int] -> Int -> Int -> Int
hashFunc lengths rounds x = value . head . drop rounds . iterate twists $ Idx 0 0 x
  where
    -- combined twists defined by knot lengths
    twists = foldr1 (.) $ map twist $ reverse lengths


-- Build hash function from a string
hashFunc' :: String -> Int -> Int -> Int
hashFunc' str = hashFunc xs
  where
    xs     = map ord str ++ suffix
    suffix = [17, 31, 73, 47, 23]


-- Compute the inverse of a permutation of [0..n-1]
invert :: [Int] -> [Int]
invert = fst . unzip . sortWith snd . zip [0..]
  where
    sortWith f = sortBy $ \a b -> compare (f a) (f b)


-- Hash a list of integers
hash :: (Int -> Int) -> [Int] -> [Int]
hash h = invert . map h


-- Convert a hash value into a dense hash value
densify :: [Int] -> [Int]
densify [] = []
densify xs = foldr1 xor chunk : densify rest
  where
    chunk = take 16 xs
    rest  = drop 16 xs


-- Convert hash to hex string
tohex :: [Int] -> String
tohex = concatMap $ printf "%02x"


main :: IO ()
main = do
  let a:b:_ = hash h1 [0..255]
  putStrLn $ "Product of first two hash elements: " ++ (show $ a*b)

  putStrLn "Example knot hashes:"
  putStrLn $ knothash test1 [0..255]
  putStrLn $ knothash test2 [0..255]
  putStrLn $ knothash test3 [0..255]
  putStrLn $ knothash test4 [0..255]

  putStrLn "Knot hash of puzzle input as string:"
  putStrLn $ knothash h2 [0..255]

  where
    -- compute a knot hash
    knothash h = tohex . densify . hash h
    -- the hash function for part 1
    h1 = hashFunc [76,1,88,148,166,217,130,0,128,254,16,2,130,71,255,229] 1
    -- test hash functions from part 2
    test1 = hashFunc' "" 64
    test2 = hashFunc' "AoC 2017" 64
    test3 = hashFunc' "1,2,3" 64
    test4 = hashFunc' "1,2,4" 64
    -- the hash function for part 2
    h2 = hashFunc' "76,1,88,148,166,217,130,0,128,254,16,2,130,71,255,229" 64
