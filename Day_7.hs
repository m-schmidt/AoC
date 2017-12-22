module Day_7 where

-- http://adventofcode.com/2017/day/7

import Control.Monad
import Control.Monad.Trans.Except
import Data.Either
import Data.Functor.Identity
import Data.List
import Data.Ord()

import qualified Data.Set as Set

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Text.ParserCombinators.Parsec


-- |Program description
data Desc = Desc { d_name   :: String
                 , d_weight :: Int
                 , d_subs   :: [String]
                 } deriving Show


-- |Parser for a single program description
program :: Parser Desc
program = do
  n    <- ident
  w    <- enparen weight
  subs <- option [] $ arrow *> ident `sepBy1` comma
  return $ Desc n w subs

  where
    ident   = many1 letter <* spaces  :: Parser String
    weight  = many1 digit <* spaces >>= return . read  :: Parser Int
    comma   = char ','  <* spaces  :: Parser Char
    arrow   = string "->"  <* spaces  :: Parser String
    enparen = between (char '(' <* spaces) (char ')' <* spaces)  :: Parser a -> Parser a


-- |Parser for a list of program descriptions
programs :: Parser [Desc]
programs = many program <* eof


-- |Name of program at root of tree
rootName :: [Desc] -> String
rootName descs = Set.elemAt 0 $ Set.difference allProgs allSubs
  where
    allProgs = Set.fromList $ map d_name descs          -- all program names
    allSubs  = Set.fromList . concat $ map d_subs descs -- all names of subprograms


-- |Mapping from program name to its description
descMap :: [Desc] -> Map String Desc
descMap = Map.fromList . map (\d -> (d_name d, d))




-- |Program tree
data Tree = Tree { t_name       :: String
                 , t_weight     :: Int     -- weight of root
                 , t_weight_acc :: Int     -- accumulated weight of root and its subtrees
                 , t_subs       :: [Tree]
                 } deriving (Show, Eq)


-- |Build tree with validation from a list of descriptions
makeTree :: ([Tree] -> Except String [Tree]) -> [Desc] -> Either String Tree
makeTree validate descs = runIdentity . runExceptT . go . description $ rootName descs
  where
    -- search map for program descriptions
    dm = Map.fromList . map (\d -> (d_name d, d)) $ descs
    -- access desciption by name
    description = (Map.!) dm
    -- sum accumulated weights of trees
    wsum = sum . map t_weight_acc
    -- recursively build and validate a tree from a description
    go (Desc n w subs) = do
      st <- (sequence . map (go . description) $ subs) >>= validate
      return $ Tree n w (w + wsum st) st


-- |Build tree and check its balance
checkTree :: [Desc] -> String
checkTree descs =
  case makeTree sameWeights descs of
    Left err -> err
    Right _  -> "The tree is balanced!"


-- |Validator that checks whether neighbored trees have the same weight
sameWeights :: [Tree] -> Except String [Tree]
sameWeights trees | ok        = return trees
                  | otherwise = throwE $ describeError trees
  where
    ok          = allEqual $ map t_weight_acc trees
    allEqual xs = and $ zipWith (==) xs (drop 1 xs)


-- |Identify the tree with an incorrect weight and compute its correct weight
describeError :: [Tree] -> String
describeError trees =
  case (map head . sortWith length . groupWith t_weight_acc . sortWith t_weight_acc) trees of
    [t_err, t_ok] -> message t_err t_ok
    _             -> undefined
  where
    -- group with comparison that applies f before actually comparing the elements
    groupWith f = groupBy (\a b -> (f a) == (f b))
    -- sort with comparison that applies f before actually comparing the elements
    sortWith f  = sortBy (\a b -> compare (f a) (f b))
    -- readable description with corrected weight
    message t_err t_ok = concat [ "'"
                                , t_name t_err
                                , "' should have weight "
                                , show (t_weight t_err + t_weight_acc t_ok - t_weight_acc t_err)
                                ]


main :: IO ()
main = do
  let input = "pbga (66) \
              \xhth (57) \
              \ebii (61) \
              \havc (66) \
              \ktlj (57) \
              \fwft (72) -> ktlj, cntj, xhth \
              \qoyq (66) \
              \padx (45) -> pbga, havc, qoyq \
              \tknk (41) -> ugml, padx, fwft \
              \jptl (61) \
              \ugml (68) -> gyxo, ebii, jptl \
              \gyxo (61) \
              \cntj (57)"
  let descs = fromRight [] $ parse (many program <* eof) "input" input
  when (null descs) $ error "invalid input"
  putStrLn $ concat ["Root: '", rootName descs, "'"]
  putStrLn $ concat ["Balance: ", checkTree descs]
