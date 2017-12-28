module Main where

-- http://adventofcode.com/2017/day/14

import Control.Monad
import Control.Monad.ST
import qualified Data.UnionFind.ST as ST

import Data.List

import qualified Day_10 as D10
import Text.Printf


-- Compute all lines of the grind
grid :: String -> [[Int]]
grid key = map knothash [0..127]
  where
    knothash n = D10.densify $ D10.hash (lineHash key n) [0..255]


-- Hash function for line n of the grid
lineHash :: String -> Int -> Int -> Int
lineHash key n = D10.hashFunc' (printf "%s-%d" key n) 64


-- Count used cells in grid defined by a key
countUsed :: String -> Int
countUsed = sum . map count . concat . grid
  where
    -- count number of one-bits in an integer
    count = length . filter (=='1') . printf "%b"


-- Converts integers to list of bit values and concatenates the lists
toBin :: [Int] -> [Bool]
toBin = concat . map (map (=='1') . printf "%08b")


-- Count distinct regions of 1 bits
countRegions :: String -> Int
countRegions key = runST $ do
  g <- (mapM . mapM) freshNode (map toBin $ grid key)
  _ <- mapM unifyNodes g
  _ <- mapM unifyNodes (transpose g)
  sum <$> mapM isRegionHead (concat g)

  where
    -- create node for a bit
    freshNode b = ST.fresh b

    -- unify adjacent 1-bits in a row
    unifyNodes (a:b:rest) = do
      da <- ST.descriptor a
      db <- ST.descriptor b
      when (da && db) $ ST.union a b
      unifyNodes (b:rest)

    unifyNodes _ = do
      return ()

    -- count regions of adjacent 1-bits
    isRegionHead bit = do
      b <- ST.descriptor bit
      h <- not <$> ST.redundant bit
      return (fromEnum $ b && h)


main :: IO ()
main = do
  let input = "flqrgnkx"
  putStrLn $ concat [ "There are ", show $ countUsed input, " used bits." ]
  putStrLn $ concat [ "There are ", show $ countRegions input, " regions." ]
