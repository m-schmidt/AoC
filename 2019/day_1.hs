-- https://adventofcode.com/2019/day/1

-- Simple Fuel requirement for mass `m`
fuel m = m `div` 3 - 2

-- Iterated fuel requirement for a mass
fuel' = sum . takeWhile (> 0) . tail . iterate fuel

main = do
  masses <- map read <$> lines <$> readFile ("day_1_input.txt")
  putStrLn $ "Total fuel requirements: " ++ (show $ sum $ map fuel masses)
  putStrLn $ "Total fuel requirements (iterated): " ++ (show $ sum $ map fuel' masses)
