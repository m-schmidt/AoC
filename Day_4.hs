module Day_4 where

import Data.List
import Control.Applicative

{-
  http://adventofcode.com/2017/day/4

  Part 1:

  A new system policy has been put in place that requires all
  accounts to use a passphrase instead of simply a password. A
  passphrase consists of a series of words (lowercase letters)
  separated by spaces.

  To ensure security, a valid passphrase must contain no duplicate
  words.

  For example:

  - `aa bb cc dd ee` is valid.

  - `aa bb cc dd aa` is not valid - the word `aa` appears more than
    once.

  - `aa bb cc dd aaa` is valid - `aa` and `aaa` count as different
    words.

  The system's full passphrase list is available as your puzzle
  input. How many passphrases are valid?

  Part 2:

  For added security, yet another system policy has been put in
  place. Now, a valid passphrase must contain no two words that
  are anagrams of each other - that is, a passphrase is invalid
  if any word's letters can be rearranged to form any other word
  in the passphrase.

  For example:

  - abcde fghij is a valid passphrase.

  - abcde xyz ecdab is not valid - the letters from the third word
    can be rearranged to form the first word.

  - a ab abc abd abf abj is a valid passphrase, because all letters
    need to be used when forming another word.

  - iiii oiii ooii oooi oooo is valid.

  - oiii ioii iioi iiio is not valid - any of these words can be
    rearranged to form any other word.

  Under this new system policy, how many passphrases are valid?
-}


-- Check for duplicate words
isValid1 :: String -> Bool
isValid1 s = length (words s) == length (nub $ words s)

countValid1 :: [String] -> Int
countValid1 = length . filter (liftA2 (&&) isValid1 isValid2)

-- Check for anagrams
isValid2 :: String -> Bool
isValid2 s = null [ w1 | w1 <- words s
                       , w2 <- words s
                       , w1 /= w2
                       , sort w1 == sort w2
                       , any (== w1) $ permutations w2 ]

countValid2 :: [String] -> Int
countValid2 = length . filter (liftA2 (&&) isValid1 isValid2)


main = do
  putStrLn $ solve
    [ "aa bb cc dd ee"
    , "aa bb cc dd aa"
    , "aa bb cc dd aaa"
    ]
  where
    solve x = concat [ "number of valid passphases = ", show . countValid1 $ x ]
