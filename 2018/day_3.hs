-- https://adventofcode.com/2018/day/3

import Data.List
import Data.Maybe
import qualified Data.Map.Strict as Map
import Text.Parsec.String
import Text.ParserCombinators.Parsec


-- Claim made by an Elf
data Claim = Claim
    { claim_id :: Int
    , px       :: Int
    , py       :: Int
    , width    :: Int
    , height   :: Int
    } deriving Show


-- Parser for Claims
parse_claim s =
  case parse p_claim  "" s of
    Right c -> c
    _       -> undefined
  where
    p_claim = do
      char '#'
      c <- p_int <* char '@'
      x <- p_int <* char ','
      y <- p_int <* char ':'
      w <- p_int <* char 'x'
      h <- p_int
      return $ Claim { claim_id=c, px=x, py=y, width=w, height=h }
    p_int = read <$> (many space *> many digit <* many space) :: Parser Int


-- All square inches of a claim
square_inches c = [ (x,y) | x <- xs, y <- ys ]
  where
    xs = [px c .. px c + width c  - 1]
    ys = [py c .. py c + height c - 1]


-- Compute mapping of square inches to claim count for a list of claims
occupation = foldr claim Map.empty
  where
    claim c m = foldr incr m $ square_inches c
    incr k m  = Map.insert k (Map.findWithDefault 0 k m + 1) m


-- Count number of multiply claimed square inches in a occupation-map `m`
count_multiples m = length . filter (> 1) . map snd . Map.toList $ m


-- Find the id of a claim that id conflict-free with others according occupation-map `m`
find_unique_claim m claims = claim_id <$> find (not . overlapping) claims
  where
    -- A claim overlaps if `m` contains a value larger than 1 for any of its squares
    overlapping = foldr f False . square_inches
    f _ True    = True
    f x False   = Map.findWithDefault 0 x m > 1


main :: IO ()
main = do
  claims <- map parse_claim <$> lines <$> readFile ("day_3_input.txt")
  let m = occupation claims
  putStrLn $ "Square inches within two or more claims: " ++ (show $ count_multiples m)
  putStrLn $ "Conflict-free claim ID: " ++ (show $ find_unique_claim m claims)
