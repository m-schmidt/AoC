module Day_8 where

-- http://adventofcode.com/2017/day/8

import Control.Monad
import qualified Control.Monad.State.Strict as S

import Data.Either

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import Text.ParserCombinators.Parsec


-- |Comparison operands for conditions
data CompareOp = LessOrEqual
               | Less
               | GreaterOrEqual
               | Greater
               | Equal
               | NotEqual
               deriving (Enum, Bounded)

instance Show CompareOp where
  show LessOrEqual    = "<="
  show Less           = "<"
  show GreaterOrEqual = ">="
  show Greater        = ">"
  show Equal          = "=="
  show NotEqual       = "!="

evalCompareOp :: CompareOp -> Int -> Int -> Bool
evalCompareOp LessOrEqual    = (<=)
evalCompareOp Less           = (<)
evalCompareOp GreaterOrEqual = (>=)
evalCompareOp Greater        = (>)
evalCompareOp Equal          = (==)
evalCompareOp NotEqual       = (/=)


-- |An operation for instructions
data InstrOp = Increment
             | Decrement
             deriving (Enum, Bounded)

instance Show InstrOp where
  show Increment = "inc"
  show Decrement = "dec"

evalInstrOp :: InstrOp -> Int -> Int -> Int
evalInstrOp Increment = (+)
evalInstrOp Decrement = (-)


-- |Instruction
data Instr = Instr { i_dst   :: String       -- ^ register updated by instruction
                   , i_op    :: Int -> Int   -- ^ update operation when executing
                   , c_src   :: String       -- ^ src register for guard
                   , c_guard :: Int -> Bool  -- ^ guard for conditional execution
                   }


-- |Parser for a single instruction
instruction :: Parser Instr
instruction = do
  i_dst   <- ident
  i_op    <- do op  <- parseEnum :: Parser InstrOp
                imm <- constant
                return $ flip (evalInstrOp op) imm
  c_src   <- string "if" *> spaces *> ident
  c_guard <- do op  <- parseEnum :: Parser CompareOp
                imm <- constant
                return $ flip (evalCompareOp op) imm
  return $ Instr i_dst i_op c_src c_guard


-- |Parse identifiers used as register names
ident :: Parser String
ident = many1 letter <* spaces


-- |Parse integer constants with optional sign
constant :: Parser Int
constant = do
  s <- option '+' $ oneOf "+-"
  n <- read <$> (many1 digit <* spaces)
  return $ if s=='+' then n else -n


-- |Parse one enumeration member as specified by its Show implementation
parseEnum :: (Show a, Enum a, Bounded a) => Parser a
parseEnum = (choice $ map mkParser [minBound..maxBound]) <* spaces
  where
    mkParser :: Show a => a -> Parser a
    mkParser x = try (string . show $ x) >> return x



-- |A register file
type Registers = Map String Int

-- |State monad with tuple of overall highscore and register file
type ST = S.State (Int, Registers)


-- |Convert parsed instruction into monadic action
execInstruction :: Instr -> ST ()
execInstruction (Instr i_dst i_op c_src c_guard) = do
  v_src <- S.gets (getRegisterValue c_src)
  when (c_guard v_src) $ do
    v_dst <- S.gets (getRegisterValue i_dst)
    S.modify (setRegisterValue i_dst $ i_op v_dst)


-- |Read register from state
getRegisterValue :: String -> (Int, Registers) -> Int
getRegisterValue k (_, m) | k `Map.member` m = m Map.! k
                          | otherwise        = 0


-- |Update state with new register value
setRegisterValue :: String -> Int -> (Int, Registers) -> (Int, Registers)
setRegisterValue k v (high, m) = (max high v, Map.insert k v m)



main :: IO ()
main = do
  let input = "b inc 5 if a > 1 \
              \a inc 1 if b < 5 \
              \c dec -10 if a >= 1 \
              \c inc -20 if c == 10"

  let instructions = fromRight [] $ map execInstruction <$> parse (many instruction <* eof) "input" input
  when (null instructions) $ error "invalid input"

  let (highScore, registerFile) = S.execState (sequence instructions) (0, Map.empty)
  putStrLn $ "Highest register value after execution: "  ++ (show . maximum . map snd . Map.toList) registerFile
  putStrLn $ "Highest register value during execution: " ++ show highScore
