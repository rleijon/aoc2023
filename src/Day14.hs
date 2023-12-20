module Day14 (run) where

import Control.Monad.State
import Data.Function (on)
import Data.List (elemIndex, findIndex, sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromJust)
import Debug.Trace (trace)

type Point = (Int, Int)

type Direction = (Int, Int)

type StateMap = Map.Map Point Char

-- 100178 too low
-- 100259 too low

run :: IO ()
run = do
  ls <- lines <$> readFile "src/day14.txt"
  let f = Map.fromList $ parseLines 0 ls
  let movedNorth = evalState (move (0, -1) (ptm f)) f
  print $ "Day 14, Part 1: " ++ show (sum (weights (length ls) movedNorth))
  let vn@(v : vs) = runCycle f []
  let target = 1000000000
  let i = fromJust (elemIndex v vs)
  let (_ : ws) = map (sum . weights (length ls)) vs
  let a = reverse (take (i + 1) ws)
  let b = reverse (drop (i + 1) ws)
  let c = b ++ cycle a
  print $ "Day 14, Part 2: " ++ show (c !! (target - 1))
  print "Done"

weights :: Int -> StateMap -> [Int]
weights l f = map (\((x, y), v) -> l - y) $ filter (\(a, b) -> b == 'O') $ Map.toList f

runCycle :: StateMap -> [StateMap] -> [StateMap]
runCycle f states = do
  let f' = runSingleCycle f
  if f' `elem` states then f' : states else runCycle f' (f' : states)

runSingleCycle :: StateMap -> StateMap
runSingleCycle f = do
  let pointsToMoveNorth = ptm f
  let f' = evalState (move (0, -1) pointsToMoveNorth) f
  let pointsToMoveWest = sortBy (compare `on` fst) $ ptm f'
  let f'' = evalState (move (-1, 0) pointsToMoveWest) f'
  let pointsToMoveSouth = reverse $ ptm f''
  let f''' = evalState (move (0, 1) pointsToMoveSouth) f''
  let pointsToMoveEast = sortBy (flip compare `on` fst) (ptm f''')
  evalState (move (1, 0) pointsToMoveEast) f'''

ptm :: StateMap -> [Point]
ptm v = sortBy (compare `on` snd) $ map fst $ filter (\(a, b) -> b == 'O') $ Map.toList v

move :: Direction -> [Point] -> State StateMap StateMap
move d [] = get
move d@(dx, dy) l@((x, y) : ps) = do
  m <- get
  let np = (x + dx, y + dy)
  let m' = Map.insert np 'O' (Map.insert (x, y) '.' m)
  case Map.lookup np m of
    Just '#' -> move d ps
    Just '.' -> put m' >> move d (np : ps)
    Just 'O' -> move d ps
    Nothing -> move d ps

parseLines :: Int -> [String] -> [(Point, Char)]
parseLines y [] = []
parseLines y (l : ls) = parseLine y 0 l ++ parseLines (y + 1) ls

parseLine :: Int -> Int -> String -> [(Point, Char)]
parseLine y x [] = []
parseLine y x (c : cs) = ((x, y), c) : parseLine y (x + 1) cs
