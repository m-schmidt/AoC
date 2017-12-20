module Day_3 where

-- http://adventofcode.com/2017/day/3


-- Coordinate (horizontal/vertical) of a cell relative to the center of the grid
data Coord = Coord Int Int deriving Show

instance Monoid Coord where
  mempty = Coord 0 0
  mappend (Coord h0 v0) (Coord h1 v1) = Coord (h0+h1) (v0+v1)

hamiltonDistance (Coord h v) = abs h + abs v

-- Coordinate delta for walking one step in a direction
left  = Coord (-1)  0
right = Coord   1   0
up    = Coord   0   1
down  = Coord   0 (-1)

-- Steps of the circular path to the n-th cell
pathToCell n = take (n-1) $ go 1
  where
    steps n | odd n     = replicate n right ++ replicate n up
            | otherwise = replicate n left  ++ replicate n down
    go n = steps n ++ go (n+1)

-- Hamilton distance of the n-th cell from the center
distance = hamiltonDistance . mconcat . pathToCell


main = do
  putStrLn $ solve 1
  putStrLn $ solve 12
  putStrLn $ solve 23
  putStrLn $ solve 1024
  where
    solve x = concat [ "distance ", show x, " = ", show . distance $ x ]
