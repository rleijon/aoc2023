{-# LANGUAGE BangPatterns #-}

module Day17 (run) where

import Control.Monad (forM_, when)
import Control.Monad.State (MonadState (put), State, evalState, get)
import Data.Char (digitToInt)
import Data.Foldable (maximumBy)
import Data.Function (on)
import Data.List (minimumBy)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Ord (comparing)
import qualified Data.Set as Set
import Debug.Trace (trace)

type Point = (Int, Int)

type Direction = (Int, Int)

type BfsState = (Set.Set (Point, Direction, Int), Point)

run :: IO ()
run = do
  graph <- Map.fromList . parseLines 0 . lines <$> readFile "src/day17.txt"
  let maxx = maximum $ map (fst . fst) $ Map.toList graph
  let maxy = maximum $ map (snd . fst) $ Map.toList graph
  let startingPoints = [((1, 0), (1, 0), graph Map.! (1, 0), 0), ((0, 1), (0, 1), graph Map.! (0, 1), 0)]
  let s = evalState (dijkstra' startingPoints (Configuration (maxx, maxy) 0 (getNewMoves graph))) Set.empty
  print $ "Day 17, Part 1: " ++ show s
  let s' = evalState (dijkstra' startingPoints (Configuration (maxx, maxy) 3 (getNewMoves' graph))) Set.empty
  print $ "Day 17, Part 2: " ++ show s'
  print "Done"

data Configuration = Configuration
  { end :: Point,
    minSteps :: Int,
    getMoves :: (Point, Direction, Int, Int) -> [(Point, Direction, Int, Int)]
  }

dijkstra' :: [(Point, Direction, Int, Int)] -> Configuration -> State (Set.Set (Point, Direction, Int)) Int
dijkstra' vs cfg = do
  let a@(p@(!x, y), d@(dx, dy), c, mom) = minimumBy (compare `on` (\(_, _, cost, _) -> cost)) vs
  let vs' = filter (/= a) vs
  seen <- get
  put (Set.insert (p, d, mom) seen)
  let action
        | p == end cfg && mom >= minSteps cfg = return c
        | Set.member (p, d, mom) seen = dijkstra' vs' cfg
        | otherwise = dijkstra' (vs' ++ getMoves cfg a) cfg
  action

getNewMoves :: Map.Map Point Int -> (Point, Direction, Int, Int) -> [(Point, Direction, Int, Int)]
getNewMoves m (p@(x, y), d@(dx, dy), c, mom) = do
  let a = if mom < 2 then Just ((x + dx, y + dy), (dx, dy), mom + 1) else Nothing
  let l = Just ((x + dy, y - dx), (dy, -dx), 0)
  let r = Just ((x - dy, y + dx), (-dy, dx), 0)
  map (\(p', d', s) -> (p', d', c + m Map.! p', s)) $ filter (\(p', _, _) -> Map.member p' m) $ catMaybes [a, l, r]

getNewMoves' :: Map.Map Point Int -> (Point, Direction, Int, Int) -> [(Point, Direction, Int, Int)]
getNewMoves' m (p@(x, y), d@(dx, dy), c, mom) = do
  let a = if mom < 9 then Just ((x + dx, y + dy), (dx, dy), mom + 1) else Nothing
  let l = if mom >= 3 then Just ((x + dy, y - dx), (dy, -dx), 0) else Nothing
  let r = if mom >= 3 then Just ((x - dy, y + dx), (-dy, dx), 0) else Nothing
  map (\(p', d', s) -> (p', d', c + m Map.! p', s)) $ filter (\(p', _, _) -> Map.member p' m) $ catMaybes [a, l, r]

parseLines :: Int -> [String] -> [(Point, Int)]
parseLines y [] = []
parseLines y (l : ls) = parseLine y 0 l ++ parseLines (y + 1) ls

parseLine :: Int -> Int -> String -> [(Point, Int)]
parseLine y x [] = []
parseLine y x (c : cs) = ((x, y), digitToInt c) : parseLine y (x + 1) cs