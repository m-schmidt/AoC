module Day_12 where

-- http://adventofcode.com/2017/day/12

import Control.Monad
import Control.Monad.ST
import qualified Data.UnionFind.ST as ST

import Data.Either
import Data.List

import Data.IntMap (IntMap)
import qualified Data.IntMap.Strict as IntMap

import Text.Parsec.String
import Text.ParserCombinators.Parsec


-- |ID record for program
data IDRec = IDRec { p_id    :: Int
                   , p_pipes :: [Int]
                   } deriving Show


-- |Parser for a single ID record
idrec :: Parser IDRec
idrec = do
  pid   <- number
  pipes <- arrow *> number `sepBy1` comma
  return $ IDRec pid pipes
  where
    number = many1 digit <* spaces >>= return . read :: Parser Int
    comma  = char ',' <* spaces :: Parser Char
    arrow  = string "<->" <* spaces :: Parser String


-- |Parser for a list of ID records
idrecs :: Parser [IDRec]
idrecs = many idrec <* eof


-- |Union find of connected programs, returns mapping of program to corresponding node
unionFind :: [IDRec] -> ST s (IntMap (ST.Point s Int))
unionFind recs = do
  nm <- IntMap.fromList <$> mapM freshNode recs
  _  <- mapM (unifyPrograms nm) recs
  return nm

  where
    freshNode (IDRec pid _) = do
      node <- ST.fresh pid
      return (pid, node)

    unifyPrograms nm (IDRec pid pipes) = do
      let p  = nm IntMap.! pid
      let ps = filter (/= p) $ map (nm IntMap.!) pipes
      mapM (ST.union p) ps


-- |Find process ids, that have a connection to process 'd'.
findGroup :: Int -> [IDRec] -> [Int]
findGroup d recs = runST $ do
  nm <- unionFind recs
  concat <$> mapM (collectEquivs nm . p_id) recs
  where
    collectEquivs nm d' = do
      let p  = nm IntMap.! d
      let p' = nm IntMap.! d'
      eq <- ST.equivalent p p'
      return $ if eq then [d'] else []


-- |Count total number of unconnected program groups.
countGroups :: [IDRec] -> Int
countGroups recs = runST $ do
  nm    <- unionFind recs
  descs <- mapM (ST.descriptor . (IntMap.!) nm . p_id) recs
  return $ length . nub $ descs


main :: IO ()
main = do
  let input = "0 <-> 2 \
              \1 <-> 1 \
              \2 <-> 0, 3, 4 \
              \3 <-> 2, 4 \
              \4 <-> 2, 3, 6 \
              \5 <-> 6 \
              \6 <-> 4, 5"

  let records = fromRight [] $ parse idrecs "input" input
  when (null records) $ error "invalid input"

  let g0 = findGroup 0 records
  putStrLn $ concat ["Group with connection to 0: ", show g0, " (", show $ length g0, " elements)"]

  let cnt = countGroups records
  putStrLn $ concat ["There are ", show cnt, " unconnected groups."]
