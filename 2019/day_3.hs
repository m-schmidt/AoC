-- https://adventofcode.com/2019/day/3

import Data.List


-- Movement vector in a wire path
data Vect = L Int | R Int | U Int | D Int

len (L n) = n
len (R n) = n
len (U n) = n
len (D n) = n

-- split a string containing comma separated words into a list of words
split_on_comma str = words $ map rep str
  where
    rep ',' = ' '
    rep ch  = ch

-- parse a string as a list of movement vectors, i.e. a path
parse_path = map p_vect . split_on_comma
  where
    p_vect ('L':n) = L $ read n
    p_vect ('R':n) = R $ read n
    p_vect ('U':n) = U $ read n
    p_vect ('D':n) = D $ read n


-- X/Y Coordinates: larger coordinates extend to the right and upwards
data Coord = Coord { c_x :: Int, c_y :: Int }

origin = Coord 0 0

manhattan_distance (Coord x y) = abs x + abs y

-- apply movement vector to a coordinate
move (Coord x y) vect =
  case vect of
    L n -> Coord (x-n) y
    R n -> Coord (x+n) y
    U n -> Coord x (y+n)
    D n -> Coord x (y-n)


-- Straight segment of a wire path: starting point and end point
data Segment = Seg { s_start :: Coord, s_end ::  Coord }

-- convert a list of movement vectors into a list of straight segments
segments vects = snd $ mapAccumL go origin vects
  where
    go p v = let q = move p v in (q, Seg p q)

-- interesting properties of segments

is_vertical seg = (c_x $ s_start seg) == (c_x $ s_end seg)

is_horizontal seg = (c_y $ s_start seg) == (c_y $ s_end seg)

covers_x seg x = min_x <= x && x <= max_x
  where
    min_x = min (c_x $ s_start seg) (c_x $ s_end seg)
    max_x = max (c_x $ s_start seg) (c_x $ s_end seg)

covers_y seg y = min_y <= y && y <= max_y
  where
    min_y = min (c_y $ s_start seg) (c_y $ s_end seg)
    max_y = max (c_y $ s_start seg) (c_y $ s_end seg)


-- Part 1

-- compute list of intersection points between two straight segments
isect s1 s2
  -- intersect vertical with horizontal line
  | is_vertical s1 && is_horizontal s2 && s2 `covers_x` s1_x && s1 `covers_y` s2_y = [ Coord s1_x s2_y ]
  -- fallback to first case by swapping arguments
  | is_horizontal s1 && is_vertical s2 = isect s2 s1
  -- assume that parallel segments never overlap
  | otherwise = []
  where
    s1_x = c_x $ s_start s1
    s2_y = c_y $ s_start s2

-- compute all intersection points of paths `pa` and `pb`
intersections pa pb =
  [ x | sa <- segments pa, sb <- segments pb, x <- isect sa sb ]

-- minimum manhattan distance of all intersections between paths `pa` and `pb`
nearest_intersection pa pb =
  minimum $ filter (/= 0) $ map manhattan_distance $ intersections pa pb


-- Part 2

-- compute the length of path `p` until the destination coordinate dx/dy is reached
length_till (Coord dx dy) p = go origin p 0
  where
    go (Coord x y) (U n:_) acc
      | dx == x && y <= dy && y + n >= dy = acc + dy - y
    go (Coord x y) (D n:_) acc
      | dx == x && y >= dy && y - n <= dy = acc + y - dy
    go (Coord x y) (R n:_) acc
      | dy == y && x <= dx && x + n >= dx = acc + dx - x
    go (Coord x y) (L n:_) acc
      | dy == y && x >= dx && x - n <= dx = acc + x - dx
    go pos (v:vs) acc
      = go (move pos v) vs (acc + len v)

-- minimum sum of lengths of two paths for a list of destination coordinates
min_combined_length pa pb dsts =
  minimum $ map (\dst -> length_till dst pa + length_till dst pb) dsts


main :: IO ()
main = do
  pa:pb:_ <- map parse_path . lines <$> readFile ("day_3_input.txt")
  putStrLn $ "Result for part 1: " ++ (show $ nearest_intersection pa pb)
  putStrLn $ "REsult for part 2: " ++ (show $ min_combined_length pa pb $ intersections pa pb)
