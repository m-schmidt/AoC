module Day_16 where

-- http://adventofcode.com/2017/day/16

import Data.List

import Text.Parsec.String
import Text.ParserCombinators.Parsec


-- |Type for dancing steps
data Step = Spin Int
          | Xchg Int Int
          | Pair Char Char
          deriving Show

-- |Parser for one dancing step
step :: Parser Step
step = choice [spin, xchg, pair] <* spaces
  where
    number = many1 digit >>= return . read
    spin   = Spin <$> (char 's' *> number)
    xchg   = do
      _ <- char 'x'
      a <- number
      _ <- char '/'
      b <- number
      return $ Xchg a b
    pair   = do
      _ <- char 'p'
      a <- oneOf startFormation
      _ <- char '/'
      b <- oneOf startFormation
      return $ Pair a b

-- |Parser for list of dancing step
steps :: Parser [Step]
steps = step `sepBy` (char ',')


-- |Apply a dancing step
apply :: String -> Step -> String
apply str st = case st of
  Spin n            -> drop (size - n) str ++ take (size - n) str
  Xchg a b | a /= b -> map (exchange (str !! a) (str !! b)) str
  Pair a b | a /= b -> map (exchange a b) str
  _                 -> str

  where
    exchange a b x | x == a    = b
                   | x == b    = a
                   | otherwise = x
    size = length startFormation


-- |Iterated application of a list of dancing steps on the starting formation
dances :: [Step] -> [String]
dances ss = iterate dance startFormation
  where
    dance x = foldl' apply x ss


-- |Determine the number of dances until the original input string reappears
cycleLength :: [Step] -> Maybe Int
cycleLength ss = (+1) <$> findIndex (==startFormation) (drop 1 $ dances ss)


-- |The starting formation
startFormation :: String
startFormation = ['a'..'e'] -- ['a'..'p']


main :: IO ()
main = do

  let input = "s1,x3/4,pe/b"
  let (Right danceSteps) = parse (steps <* eof) "input" input

  let (Just clen) = cycleLength danceSteps
  putStrLn $ concat [ "After ", show clen, " dances, the starting formation is reached again." ]

  let after1Formation = (dances danceSteps) !! 1
  let endFormation = (dances danceSteps) !! (1000000000 `mod` clen)

  putStrLn $ concat [ "Start formation: ", show startFormation ]
  putStrLn $ concat [ "After one dance: ", show after1Formation ]
  putStrLn $ concat [ "Final formation: ", show endFormation ]
