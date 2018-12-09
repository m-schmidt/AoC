module Day_4 where

-- http://adventofcode.com/2017/day/4

import Data.List
import Control.Applicative


-- |Check for duplicate words
isValid1 :: String -> Bool
isValid1 s = length (words s) == length (nub $ words s)

countValid1 :: [String] -> Int
countValid1 = length . filter (liftA2 (&&) isValid1 isValid2)

-- |Check for anagrams
isValid2 :: String -> Bool
isValid2 s = null [ w1 | w1 <- words s
                       , w2 <- words s
                       , w1 /= w2
                       , sort w1 == sort w2
                       , any (== w1) $ permutations w2 ]

countValid2 :: [String] -> Int
countValid2 = length . filter (liftA2 (&&) isValid1 isValid2)


main :: IO ()
main = do
  putStrLn $ solve
    [ "aa bb cc dd ee"
    , "aa bb cc dd aa"
    , "aa bb cc dd aaa"
    ]
  where
    solve x = concat [ "number of valid passphases = ", show . countValid1 $ x ]
