{-# LANGUAGE DeriveFunctor #-}

-- https://adventofcode.com/2019/day/7

import Data.List
import Recursion
import Day_5 (run, parse, Memory)


-- Recursive tree for unfolding amplification configurations
data TreeF a = NodeF Int [a] deriving Functor
type Tree = Fix TreeF

type Seed = Int
type Pool = [Int]

build_config :: Memory -> Coalgebra TreeF (Seed, Pool)
build_config mem (s, p) = NodeF s [(t, delete x p) | x <- p, let [t] = run mem [x, s]]

-- Part 1: compute highest signal
max_signal (NodeF s [])  = s
max_signal (NodeF _ lst) = maximum lst


main = do
  initial_mem <- parse <$> readFile ("day_7_input.txt")
  putStrLn $ "Part 1: " ++ (show $ hylo max_signal (build_config initial_mem) (0, [0..4]))
