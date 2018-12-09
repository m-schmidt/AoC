-- https://adventofcode.com/2018/day/5

import Data.Char
import Data.List

-- Apply reactions to a polymer chain
react :: String -> String
react s = reverse $ foldl' f [] s
  where
    f [] c = [c]
    f (x:xs) c
      | conflict x c = xs
      | otherwise    = c:x:xs

    conflict a b = a /= b && toUpper a == toUpper b

-- Compute minimum of a list according a weighting function `w`
minimum_by1 _ [] = undefined
minimum_by1 w xs = fst $ foldr1 f pairs
  where
    f a b | snd a <= snd b = a
          | otherwise      = b
    pairs = fmap (\x -> (x, w x)) xs

-- All reactions excluding single elements
all_reactions s = [ (c, react . remove c $ s) | c <- ['a'..'z'] ]
  where
    remove p           = filter (different_from p)
    different_from a b = toUpper a /= toUpper b

-- Element that leads to smallest remaining polymer after removal
smallest = length . snd . minimum_by1 (length . snd) . all_reactions


main :: IO ()
main = do
  polymer <- filter isAlpha <$> readFile ("day_5_input.txt")
  putStrLn $ "Remaining units: " ++ (show $ length $ react polymer)
  putStrLn $ "Unit to be removed: " ++ (show $ smallest polymer)
