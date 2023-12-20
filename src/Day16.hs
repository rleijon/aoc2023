{-# LANGUAGE BangPatterns #-}

module Day16 (run) where

import Control.Monad.State
import qualified Data.IntMap as Seen
import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Debug.Trace (trace)

type Point = (Int, Int)

type Direction = (Int, Int)

type Beam = (Point, Direction)

run :: IO ()
run = do
  ls <- Map.fromList . parseLines 0 . lines <$> readFile "src/day16.txt"
  let startBeam = ((-1, 0), (1, 0))
  let v = lightStepAll'' [startBeam] ls Set.empty
  let v' = nub $ map fst $ Set.toList v
  print $ "Day 16, Part 1: " ++ show (length v' - 1)
  let maxx = maximum $ map (fst . fst) (Map.toList ls)
  let maxy = maximum $ map (snd . fst) (Map.toList ls)
  let allPoints =
        [((-1, y), (1, 0)) | y <- [0 .. maxy]]
          ++ [((maxx + 1, y), (-1, 0)) | y <- [0 .. maxy]]
          ++ [((x, -1), (0, 1)) | x <- [0 .. maxx]]
          ++ [((x, maxy + 1), (0, -1)) | x <- [0 .. maxx]]
  let l = map ((length . nub . map fst) . (\p -> Set.toList $ lightStepAll'' [p] ls Set.empty)) allPoints
  print $ "Day 16, Part 2: " ++ show (maximum l - 1)
  print "Done"

lightStepAll'' :: [Beam] -> Map.Map Point Char -> Set.Set Beam -> Set.Set Beam
lightStepAll'' [] m s = s
lightStepAll'' (h : hs) m s
  | Set.member h s = lightStepAll'' hs m s
  | otherwise = do
      let nb = lightStep h m
      lightStepAll'' (nb ++ hs) m (Set.insert h s)

lightStep :: Beam -> Map.Map Point Char -> [Beam]
lightStep ((!x, !y), d@(!dx, !dy)) m = case Map.lookup np m of
  Just '.' -> [(np, d)]
  Just '/' -> [(np, (-dy, -dx))]
  Just '\\' -> [(np, (dy, dx))]
  Just '-' -> if dy == 0 then [(np, d)] else [(np, (-1, 0)), (np, (1, 0))]
  Just '|' -> if dx == 0 then [(np, d)] else [(np, (0, -1)), (np, (0, 1))]
  Nothing -> []
  where
    !np = (x + dx, y + dy)

parseLines :: Int -> [String] -> [(Point, Char)]
parseLines y [] = []
parseLines y (l : ls) = parseLine y 0 l ++ parseLines (y + 1) ls

parseLine :: Int -> Int -> String -> [(Point, Char)]
parseLine y x [] = []
parseLine y x (c : cs) = ((x, y), c) : parseLine y (x + 1) cs