-- https://adventofcode.com/2018/day/2

import Data.List

to_int True  = 1
to_int False = 0

-- Groups and counts identical letters in an ID
letter_counts = map length . group . sort

-- Checks whether a list contains `n`
contains n = to_int . any ((==) n)

-- Checksum for a list of IDs
checksum ids = cnt 2 * cnt 3
  where
    cnt n = sum $ map (contains n . letter_counts) ids

-- The distance between two IDs is the number of different letters
distance a b = sum $ zipWith ((to_int .) . (/=)) a b

-- List of ID pairs that are 'very close' together
close_pairs ids = [ (x,y) | (x:ys) <- tails ids, y <- ys, distance x y == 1 ]

-- Extract common letters from two IDs
common_letters (a, b) = map fst $ filter (uncurry (==)) $ zip a b

main :: IO ()
main = do
  box_ids <- lines <$> readFile ("day_2_input.txt")
  putStrLn $ "Checksum: " ++ (show $ checksum box_ids)
  putStrLn $ "Commpn letters: " ++ (common_letters . head . close_pairs $ box_ids)
