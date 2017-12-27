module Day_13 where

-- http://adventofcode.com/2017/day/13

import Data.Either
import Data.List
import Data.Maybe

import Text.Parsec.String
import Text.ParserCombinators.Parsec


-- |Parse input into pairs of depth and range
records :: Parser [(Int, Int)]
records = many record <* eof
   where
    number = many1 digit <* spaces >>= return . read
    colon  = char ':' <* spaces
    record = do d <- number
                r <- colon *> number
                return $ (d, r)


-- |Check whether a single filter catches a delayed packet
catches :: Int -> (Int, Int) -> Bool
catches _     (_, 0)         = True
catches delay (depth, range) = step `mod` cycletime == 0
  where
    step      = depth + delay
    cycletime = 2 * (range - 1)


-- |Compute severity for a ride
severity :: Int -> [(Int, Int)] -> Int
severity delay = sum . map sev . filter (catches delay)
  where
    sev (depth, range) = depth * range


-- |Check whether a packet with delay gets caught by the filter
caught :: [(Int, Int)] -> Int -> Bool
caught recs delay = not . null $ (filter (catches delay) recs)


-- |Find minimum delay such that severity is zero
findDelay :: [(Int, Int)] -> Int
findDelay recs = fromJust $ elemIndex False $ map (caught recs) [0..]



main :: IO ()
main = do
  let input = "0: 3 \
              \1: 2 \
              \4: 4 \
              \6: 4"

  let recs = fromRight [] $ parse records "input" input
  putStrLn $ solve  0 recs
  putStrLn $ solve (findDelay recs) recs

  where
    solve d recs = concat [ "Severity for delay ", show d, " is ", show $ severity d recs, " (caught: ",  show $ caught recs d, ")" ]
