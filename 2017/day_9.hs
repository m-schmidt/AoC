module Day_9 where

-- http://adventofcode.com/2017/day/9

import Control.Monad
import Data.Either
import Text.Parsec.String
import Text.ParserCombinators.Parsec


group :: Int -> Parser (Int, Int)
group n = do
  (depths, count) <- unzip <$> between (char '{') (char '}') (groupContent (n+1) `sepBy` (char ','))
  return $ (n + sum depths, sum count)

garbage :: Parser (Int, Int)
garbage = do
  chars <- between (char '<') (char '>') (many $ canceled <|> notcanceled)
  return (0, sum chars)
  where
    canceled    = char '!' *> anyChar >> return 0
    notcanceled = noneOf "!>" >> return 1

groupContent :: Int -> Parser (Int, Int)
groupContent n = group n <|> garbage


main :: IO ()
main = do
  putStrLn "Part1:"
  solve "{}"
  solve "{{{}}}"
  solve "{{},{}}"
  solve "{{{},{},{{}}}}"
  solve "{<a>,<a>,<a>,<a>}"
  solve "{{<ab>},{<ab>},{<ab>},{<ab>}}"
  solve "{{<!!>},{<!!>},{<!!>},{<!!>}}"
  solve "{{<a!>},{<a!>},{<a!>},{<ab>}}"

  putStrLn "Part2:"
  solve "{<>}"
  solve "{<random characters>}"
  solve "{<<<<>}"
  solve "{<{!>}>}"
  solve "{<!!>}"
  solve "{<!!!>>}"
  solve "{<{o\"i!a,<{i<a>}"

  where
    solve s = putStrLn $ "score / garbage-chars: " ++ (show $ fromRight (-1,-1) $ parse (group 1 <* eof) "input" s)
