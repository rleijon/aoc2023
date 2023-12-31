{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE BangPatterns #-}

module Day23 where

import Control.Monad.State
import Data.Bifunctor
import Data.ByteString (sort)
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (trace)

type Point = (Int, Int)

type Path = [Point]

run :: IO ()
run = do
  ls <- Map.fromList . parseLines 0 . lines <$> readFile "src/day23.txt"
  let maxx = maximum $ map (fst . fst) $ Map.toList ls
  let maxy = maximum $ map (fst . fst) $ Map.toList ls
  let v = evalState (bfs (maxx - 1, maxy) [((1, 0), [])] ls) []
  let vl = map length v
  print $ "Day 23, Part 1: " ++ show (maximum vl)

  let crossings = (1, 0) : (maxx - 1, maxy) : getCrossings ls (Map.toList ls)
  let edges = [(c, getEdges [(c, [])] c ls (filter (/= c) crossings) []) | c <- crossings]
  let edges' = Map.fromList $ map (second (map (second length))) edges
  let res = evalState (bfs' (maxx - 1, maxy) [((1, 0), [])] edges') []
  -- print (map reverse res)
  let vs = map (\p -> lengthOfPath (reverse ((maxx - 1, maxy) : p)) edges') res
  -- let v' = evalState (bfs' (maxx - 1, maxy) [((1, 0), Set.empty)] ls) 0
  print $ "Day 23, Part 2: " ++ show (maximum vs)
  print "Done"

lengthOfPath :: [Point] -> Map.Map Point [(Point, Int)] -> Int
lengthOfPath [] m = 0
lengthOfPath [x] m = 0
lengthOfPath (x : y : xs) m = do
  let a = maximum $ map snd $ filter (\(t, c) -> t == y) $ m Map.! x
  -- trace (show (x, y, a))
  a + lengthOfPath (y : xs) m

bfs' :: (Int, Int) -> [(Point, [Point])] -> Map.Map Point [(Point, Int)] -> State [Path] [Path]
bfs' end [] m = get
bfs' end ((p, path) : ps) m
  | p == end = do
      ps' <- get
      put (path : ps')
      bfs' end ps m
  | otherwise = do
      let f = filter (`notElem` path) (map fst $ m Map.! p)
      let np = map (\v -> (v, p : path)) f
      bfs' end (np ++ ps) m

getEdges :: [(Point, [Point])] -> Point -> Map.Map Point Char -> [Point] -> [(Point, Path)] -> [(Point, Path)]
getEdges [] point mp crossings found = found
getEdges ((p, pth) : xs) point mp crossings found
  | p `elem` crossings = getEdges xs point mp crossings ((p, pth) : found)
  | otherwise = do
      let f = getNeighbors (p, pth) mp
      let np = map (\v -> (v, p : pth)) f
      getEdges (np ++ xs) point mp crossings found

getCrossings :: Map.Map Point Char -> [(Point, Char)] -> [Point]
getCrossings m [] = []
getCrossings m (((x, y), '#') : ps) = getCrossings m ps
getCrossings m (((x, y), v) : ps) = do
  let z = length $ filter (\v -> Map.member v m && m Map.! v /= '#') [(x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1)]
  if z > 2
    then (x, y) : getCrossings m ps
    else getCrossings m ps

getNeighbors' :: (Point, Set.Set Point) -> Map.Map Point Char -> [Point]
getNeighbors' ((!x, !y), !path) m =
  filter
    (\v -> Map.member v m && m Map.! v /= '#' && Set.notMember v path)
    [(x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1)]

bfs :: (Int, Int) -> [(Point, [Point])] -> Map.Map Point Char -> State [Path] [Path]
bfs end [] m = get
bfs end ((p@(x, y), path) : ps) m
  | p == end = do
      ps' <- get
      put (path : ps')
      bfs end ps m
  | m Map.! p == '>' = if (x + 1, y) `elem` path then bfs end ps m else bfs end (((x + 1, y), p : path) : ps) m
  | m Map.! p == 'v' = if (x, y + 1) `elem` path then bfs end ps m else bfs end (((x, y + 1), p : path) : ps) m
  | otherwise = do
      let f = getNeighbors (p, path) m
      let np = map (\v -> (v, p : path)) f
      bfs end (np ++ ps) m

getNeighbors :: (Point, Path) -> Map.Map Point Char -> [Point]
getNeighbors ((x, y), path) m =
  filter
    (\v -> Map.member v m && m Map.! v /= '#' && v `notElem` path)
    [(x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1)]

parseLines :: Int -> [String] -> [(Point, Char)]
parseLines y [] = []
parseLines y (l : ls) = parseLine y 0 l ++ parseLines (y + 1) ls

parseLine :: Int -> Int -> String -> [(Point, Char)]
parseLine y x [] = []
parseLine y x (c : cs) = ((x, y), c) : parseLine y (x + 1) cs