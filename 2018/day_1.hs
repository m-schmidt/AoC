-- https://adventofcode.com/2018/day/1

import Data.List (mapAccumL)
import Data.Set (empty, member, insert)

-- All frequencies resulting from finite prefixes of the circular frequency changes
frequencies chgs = snd $ mapAccumL add 0 $ cycle chgs
  where
    add acc d = (acc+d, acc+d)

-- Find first duplicate in a list
first_duplicate xs = go empty xs
  where
    go _   []     = Nothing
    go acc (x:xs) = if x `member` acc then Just x else go (insert x acc) xs

-- Parse string to integer
to_int :: String -> Int
to_int ('+':num) = read num
to_int num       = read num

main :: IO ()
main = do
  changes <- map to_int <$> lines <$> readFile ("day_1_input.txt")
  putStrLn $ "Resulting frequency: " ++ (show $ sum changes)
  putStrLn $ "First frequency reached twice: " ++ (show . first_duplicate $ frequencies changes)
