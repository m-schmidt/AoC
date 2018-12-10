-- https://adventofcode.com/2018/day/8

import Data.List
import Data.Maybe
import Data.Vector (fromList, (!?))


data Tree = Tree Int Int [Tree] [Int]

-- Parse a single tree from the front of the input
parse_tree (nc:nm:s) =
  let (cs, s')  = parse_n_trees nc s in
  let (ms, s'') = splitAt nm s' in
  (Tree nc nm cs ms, s'')

-- Parse n trees from the front of the input
parse_n_trees 0 s = ([], s)
parse_n_trees n s =
  let (c, s')   = parse_tree s in
  let (cs, s'') = parse_n_trees (n-1) s' in
  (c:cs, s'')


-- Checksum is the sum of all metadata
checksum (Tree _ _ cs ms) = sum ms + (sum $ map checksum cs)


-- Value of a node is sum of metadata or sum of children indexed by metadata
value (Tree 0  _ [] ms) = sum ms
value (Tree nc _ cs ms) =
    let vcs = fromList cs in
    sum $ map value $ map (fromMaybe empty_tree . (!?) vcs . subtract 1) ms
  where
    empty_tree = Tree 0 0 [] []


main :: IO ()
main = do
  tree <- fst . parse_tree . map read . words <$> readFile ("day_8_input.txt")
  putStrLn $ "Sum of metadata: " ++ (show $ checksum tree)
  putStrLn $ "Value of root: " ++ (show $ value tree)
