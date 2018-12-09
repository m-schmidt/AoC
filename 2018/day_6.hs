-- https://adventofcode.com/2018/day/6

import Data.List
import Data.Maybe
import qualified Data.Set as Set


-- Point definition
data Point = Point
  { px :: Int
  , py :: Int
  } deriving Show

manhattan_distance p1 p2 = abs (px p1 - px p2) + abs (py p1 - py p2)


-- Named coordinates
data Coord = Coord
  { name  :: Char
  , point :: Point
  } deriving Show

parse_coord :: Char -> String -> Coord
parse_coord c s = Coord c $ Point x y
  where
    (x, y) = read $ "(" ++ s ++ ")" :: (Int, Int)


-- Rectangles
data Rect = Rect
  { p1 :: Point
  , p2 :: Point
  } deriving Show

on_edge_of p r = equal px || equal py
  where
    equal f = f p == f (p1 r) || f p == f (p2 r)


-- Smallest rectangle enclosing all coordinates `cs`
enclosing_rect cs = Rect (Point min_x min_y) (Point max_x max_y)
  where
    (min_x, max_x) = range (px . point <$> cs)
    (min_y, max_y) = range (py . point <$> cs)
    range ps       = (minimum ps, maximum ps)


-- List of all points contained in a rectangle
locations r = [ Point x y | x <- xs, y <- ys ]
  where
    xs = [px (p1 r)..px (p2 r)]
    ys = [py (p1 r)..py (p2 r)]


-- Compute name of nearest coordinate in `cs` for point `p`
nearest p cs = fst $ foldr f (Nothing, maxBound) cs
  where
    f c (r, d) =
      case compare distance d of
        LT -> (Just (name c), distance)
        EQ -> (Nothing, d)
        GT -> (r, d)
      where
        distance = manhattan_distance (point c) p


-- Compute nearest named coordinate for all points of the enclosing rectangle defined by parsed coordinates
nearest_neighbors cs =
  [Coord (fromJust n) p | p <- locations (enclosing_rect cs)
                        , let n = nearest p cs
                        , n /= Nothing ]


-- Compute locations with sum of distances to coordinates below threshold
common_region cs =
  [ p | p <- locations (enclosing_rect cs)
      , sum (map (manhattan_distance p . point) cs) < 10000 ]


main :: IO ()
main = do
  coordinates <- zipWith parse_coord ['A'..] . lines <$> readFile ("day_6_input.txt")
  let nn = nearest_neighbors coordinates
  let rr = enclosing_rect coordinates

  let all_regions      = Set.fromList $ map name coordinates
  let infinite_regions = Set.fromList $ map name $ filter (\c -> (point c) `on_edge_of` rr) nn
  let finite_regions   = Set.difference all_regions infinite_regions

  -- Throw away all nearest neighbor info for infinite regions
  let finite_nn    = filter (flip Set.member finite_regions) $ map name nn
  -- Search largest remaining group and count its size
  let largest_area = maximum $ map length $ group $ sort finite_nn

  putStrLn $ "Size of largest finite area: " ++ (show $ largest_area)
  putStrLn $ "Size of common region: " ++ (show $ length $ common_region coordinates)
