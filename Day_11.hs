module Day_11 where

-- http://adventofcode.com/2017/day/11

import Data.Either
import Data.List
import Text.Parsec.String
import Text.ParserCombinators.Parsec


-- |Move directions in the hex grid
data HexDirection
  = North
  | NorthWest
  | SouthWest
  | South
  | SouthEast
  | NorthEast
  deriving (Eq, Enum, Bounded, Ord)

instance Show HexDirection where
  show North     = "n"
  show NorthWest = "nw"
  show SouthWest = "sw"
  show South     = "s"
  show SouthEast = "se"
  show NorthEast = "ne"


-- |Parser for hex directions, derived from show instance
hexDirection :: Parser HexDirection
hexDirection = (choice $ map mkParser allDirections) <* spaces
  where
    mkParser x    = try (string . show $ x) >> return x
    allDirections = sortWithRev show [minBound..maxBound]

hexDirections :: Parser [HexDirection]
hexDirections = hexDirection `sepBy` (char ',')


-- |Sort a list by applying a function before comparing the elements
sortWithRev :: Ord b => (a -> b) -> [a] -> [a]
sortWithRev f = sortBy $ \a b -> compare (f b) (f a)


-- |x/y/z coordinate in 3D space
type Coord = (Int, Int, Int)

move :: HexDirection -> Coord -> Coord
move dir (x, y, z) = case dir of
  North     -> (x,     y,     z + 1)
  South     -> (x,     y,     z - 1)
  SouthWest -> (x,     y + 1, z    )
  NorthEast -> (x,     y - 1, z    )
  SouthEast -> (x + 1, y,     z    )
  NorthWest -> (x - 1, y,     z    )


-- |Shifting coordinates along the x=y=z axis
shift :: Coord -> Int -> Coord
shift (x, y, z) d = (x - d, y - d, z - d)

-- |Norm for coordinates
norm :: Coord -> Int
norm (x, y, z) = abs x + abs y + abs z

-- |Minimize norm of coordinates
minimize :: Coord -> Int
minimize coord@(x, y, z) =
  let mid = sort [x, y, z] !! 1
  in norm $ shift coord mid



main :: IO ()
main = do
  putStrLn $ solve $ path test1
  putStrLn $ solve $ path test2
  putStrLn $ solve $ path test3
  putStrLn $ solve $ path test4

  where
    -- parse input to list of move directions
    path str = fromRight [] $ parse (hexDirections <* eof) "input" str
    -- compute distance from center at end of path
    distance = minimize . foldr move (0, 0, 0)
    -- compute maximum distance from center at any point of the path
    maxDistance = maximum . map distance . inits

    -- example inputs
    solve p = concat [ "Distance: ", show $ distance p, ", max distance during walk: ", show $ maxDistance p ]

    test1 = "ne,ne,ne"
    test2 = "ne,ne,sw,sw"
    test3 = "ne,ne,s,s"
    test4 = "se,sw,se,sw,sw"
