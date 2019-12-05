-- https://adventofcode.com/2019/day/5

import qualified Data.Map.Strict as Map


-- use a map as random access memory
type Memory = Map.Map Int Int

-- create initial memory state
fromList = Map.fromList . zip [0..]

-- read in memory `m` at address `a`
get m a = Map.findWithDefault 0 a m

-- update memory `m` at address `a` to value `v`
set m a v = Map.insert a v m


-- raw execution of a program
execute (pc, m, inp, out) =
  case opcode of
    -- addition
    1  -> execute (pc + 4, set m dst3 $ src1+src2, inp, out)
    -- multiplication
    2  -> execute (pc + 4, set m dst3 $ src1*src2, inp, out)
    -- input
    3  -> execute (pc + 2, set m dst1 $ head inp, tail inp, out)
    -- output
    4  -> execute (pc + 2, m, inp, src1:out)
    -- jump-if-true
    5  -> execute (if src1 /= 0 then src2 else pc + 3, m, inp, out)
    -- jump-if-false
    6  -> execute (if src1 == 0 then src2 else pc + 3, m, inp, out)
    -- is less than
    7  -> execute (pc + 4, set m dst3 $ if src1 < src2 then 1 else 0, inp, out)
    -- is equal
    8  -> execute (pc + 4, set m dst3 $ if src1 == src2 then 1 else 0, inp, out)
    -- halt
    99 -> (pc, m, inp, out)
  where
    -- opcode of current instruction
    ins    = get m (pc + 0)
    opcode = ins `mod` 100
    -- addressing mode of operand n
    is_imm n = (ins `div` (10 ^ (n+1)) `mod` 10) /= 0
    -- access operands as immediate/address or with indirection
    operand n imm = let v = get m (pc + n) in if imm then v else get m v
    -- source operands have addressing modes
    src1 = operand <*> is_imm $ 1
    src2 = operand <*> is_imm $ 2
    -- destination operands are always addresses
    dst1 = operand 1 True
    dst3 = operand 3 True

-- run a program with input and return its output
run p inp =
    let (_, _, _, out) = execute (0, p, inp, []) in
    reverse out


-- split a string containing comma separated words into a list of words
split_on_comma str = words $ map (\ch -> if ch == ',' then ' ' else ch) str

-- parse input as list of comma separated integers
parse = fromList . map read . split_on_comma


main :: IO ()
main = do
  initial_mem <- parse <$> readFile ("day_5_input.txt")
  putStrLn $ "Result for part 1: " ++ (show $ dropWhile (==0) $ run initial_mem [1])
  putStrLn $ "Result for part 2: " ++ (show $ run initial_mem [5])
