-- https://adventofcode.com/2018/day/7

import Data.Char
import Data.List
import Data.Function ((&))
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set


-- Task duration in seconds
duration t = 61 + (ord t - ord 'A')

-- Slots for schduling
slots = 5


data Constraint = Constraint Char Char
  deriving Show

parse_constraint s = Constraint (s!!5) (s!!36)


-- Dependency Map

-- Ensure a value for key `k` in map `m`
ensure k m = Map.alter ensr k m
  where
    ensr Nothing  = Just 0
    ensr v        = v

-- Increment value of key `k` in map `m` or introduce it with value 1
increment k m = Map.alter incr k m
  where
    incr (Just v) = Just $ v+1
    incr Nothing  = Just 1

-- Decrement value of key `k` in map `m`
decrement k m = Map.update decr k m
  where
    decr v | v > 0     = Just $ v-1
           | otherwise = undefined

-- Build initial task-map from constraints
task_map cs = foldr f Map.empty cs
  where
    f (Constraint a b) = ensure a . increment b

-- Get up to `n` unconstrained tasks from a task-map `m`
find_tasks n s m = Map.toList m
                    & filter (\(x,_) -> not (x `elem` s))
                    & filter ((==0) . snd)
                    & map fst
                    & sort
                    & take n

-- Update task-map by removing tasks
remove_tasks cs ts m = foldr f (foldr Map.delete m ts) cs
  where
    f (Constraint a b) | a `elem` ts = decrement b
                       | otherwise   = id



-- Compute order of tasks for single worker
compute_order cs m =
  case find_tasks 1 "" m of
    [t] -> t:(compute_order cs $ remove_tasks cs [t] m)
    _   -> []


-- Scheduler for cooperative work, returns elapsed time
schedule cs e wl m =
  -- number of free slots
  let fs = slots - length wl in
  -- determine tasks for free slots
  let ts = find_tasks fs (map fst wl) m in
  -- put tasks into slots
  let wl' = wl ++ map (\t -> (t, duration t)) ts in
  -- retire next tasks
  case finish_tasks wl' of
    Just (t, fts, wl'') -> schedule cs (e+t) wl'' (remove_tasks cs fts m)
    Nothing             -> e

  where
    finish_tasks [] = Nothing
    finish_tasks wl =
      -- worklist sorted by remaining duration
      let wl' = sortOn snd wl in
      -- minimum of remaining task durations
      let t = snd $ head wl' in
      -- compute duration, next tasks to finish and updated worklist
      let (p1, p2) = partition ((==t) . snd) wl' in
      let next = map fst p1 in
      let wl'' = map (\(a, b) -> (a, b-t)) p2 in
      Just (t, next, wl'')


main :: IO ()
main = do
  constraints <- map parse_constraint . lines <$> readFile ("day_7_input.txt")
  let dm = task_map constraints
  putStrLn $ "Task order: " ++ (compute_order constraints dm)
  putStrLn $ "Completing all steps takes: " ++ (show $ schedule constraints 0 [] dm) ++ " seconds."
