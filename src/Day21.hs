{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Day21 where

import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Set as Set

run :: IO ()
run = do
  ls <- Map.fromList . parseLines 0 . lines <$> readFile "src/day21.txt"
  maxd <- (\x -> read x :: Int) <$> readFile "src/day21maxd.txt"
  let sp = fst $ head $ filter (\x -> snd x == 'S') $ Map.toList ls
  let v = evalState (bfs maxd [(sp, 0)] ls) Set.empty
  let c = filter (\a -> snd a == maxd) $ Set.toList v
  print $ "Day 21, Part 1: " ++ show (length c)
  size <- length . lines <$> readFile "src/day21.txt"
  let a0 = evalState (bfs' size (size `div` 2) [(sp, 0)] ls) Set.empty
  let a1 = evalState (bfs' size (3 * size `div` 2) [(sp, 0)] ls) Set.empty
  let a2 = evalState (bfs' size (5 * size `div` 2) [(sp, 0)] ls) Set.empty
  let x = (26501365 - (size `div` 2)) `div` size
  let q = (a2 - (2 * a1) + a0) `div` 2
  let p = a1 - a0 - q
  let r = q * x * x + p * x + a0
  print $ "Day 21, Part 2: " ++ show r
  print "Done"

type Point = (Int, Int)

type DistanceMap = Set.Set (Point, Int)

bfs' :: Int -> Int -> [(Point, Int)] -> Map.Map Point Char -> State DistanceMap Int
bfs' size maxd [] m = gets (Set.size . Set.filter (\x -> snd x == maxd))
bfs' size maxd ((p, d) : ps) m
  | d > maxd = bfs' size maxd ps m
  | otherwise = do
      seen <- get
      if Set.member (p, d) seen
        then bfs' size maxd ps m
        else do
          put (Set.insert (p, d) seen)
          let a = map (\x -> (x, d + 1)) $ getNeighbors' size p m
          bfs' size maxd (a ++ ps) m

getNeighbors' :: Int -> Point -> Map.Map Point Char -> [Point]
getNeighbors' size (x, y) m =
  filter
    (\(x', y') -> m Map.! (x' `mod` size, y' `mod` size) /= '#')
    [(x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1)]

bfs :: Int -> [(Point, Int)] -> Map.Map Point Char -> State DistanceMap DistanceMap
bfs maxd [] m = get
bfs maxd ((p, d) : ps) m
  | d > maxd = bfs maxd ps m
  | otherwise = do
      seen <- get
      if Set.member (p, d) seen
        then bfs maxd ps m
        else do
          put (Set.insert (p, d) seen)
          let a = map (\x -> (x, d + 1)) $ getNeighbors p m
          bfs maxd (a ++ ps) m

getNeighbors :: Point -> Map.Map Point Char -> [Point]
getNeighbors (x, y) m =
  filter
    (\x -> Map.member x m && m Map.! x /= '#')
    [(x + 1, y), (x - 1, y), (x, y - 1), (x, y + 1)]

parseLines :: Int -> [String] -> [(Point, Char)]
parseLines y [] = []
parseLines y (l : ls) = parseLine y 0 l ++ parseLines (y + 1) ls

parseLine :: Int -> Int -> String -> [(Point, Char)]
parseLine y x [] = []
parseLine y x (c : cs) = ((x, y), c) : parseLine y (x + 1) cs