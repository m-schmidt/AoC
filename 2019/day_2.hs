-- https://adventofcode.com/2019/day/2

import qualified Data.Map.Strict as Map
import Data.Function (fix)


-- use a map as random access memory
type Memory = Map.Map Int Int

-- create initial memory state
fromList = Map.fromList . zip [0..]

-- read in memory `m` at address `a`
get m a = Map.findWithDefault 0 a m

-- update memory `m` at address `a` to value `v`
set a v m = Map.insert a v m


-- raw execution of a program using open recursion
execute m = fix exec (0, m)

exec rec (pc, m) =
  case op of
    -- addition
    1  -> rec (pc + 4, set d (v1 + v2) m)
    -- multiplication
    2  -> rec (pc + 4, set d (v1 * v2) m)
    -- halt
    99 -> (pc, m)
  where
    -- opcode of current instruction
    op = get m (pc + 0)
    -- memory operands 1 and 2
    v1 = get m $ get m (pc + 1)
    v2 = get m $ get m (pc + 2)
    -- destination address
    d  = get m (pc + 3)


-- run a program with applied patch `noun` and `verb` and return memory cell 0 as result
run noun verb = result . execute . patch
  where
    patch m       = set 1 noun $ set 2 verb $ m
    result (_, m) = get m 0


-- parse input as list of comma separated integers
parse = fromList . map read . words . map update
  where
    update ',' = ' '
    update ch  = ch


main :: IO ()
main = do
  initial_mem <- parse <$> readFile ("day_2_input.txt")
  let p1 = run 12 2 initial_mem
  let p2 = [ n * 100 + v | n <- [0..99], v <- [0..99], run n v initial_mem == 19690720 ]
  putStrLn $ "Result for part 1: " ++ (show p1)
  putStrLn $ "Result for part 2: " ++ (show $ head p2)
