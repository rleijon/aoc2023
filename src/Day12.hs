module Day12 (run) where

import Control.Monad.Memo (Memo, memo, startEvalMemo)
import Data.List (intercalate)
import Data.List.Split (splitOn)
import Debug.Trace (trace)

type Input = (String, [Int])

run :: IO ()
run = do
  ls <- map parseLine . lines <$> readFile "src/day12.txt"
  let fc = map (\(s, c) -> (s, startEvalMemo $ findCombinations'' (s, c))) ls
  print $ "Day 12, Part 1: " ++ show (sum $ map snd fc)
  let ls' = map (\(s, c) -> (intercalate "?" $ replicate 5 s, concat $ replicate 5 c)) ls
  let fc' = map (\(s, c) -> (s, startEvalMemo $ findCombinations'' (s, c))) ls'
  print $ "Day 12, Part 2: " ++ show (sum $ map snd fc')
  print "Done"

findCombinations'' :: Input -> Memo Input Int Int
findCombinations'' ([], cs) = if null cs then return 1 else return 0
findCombinations'' (s, []) = if '#' `notElem` s then return 1 else return 0
findCombinations'' (ps@('.' : ss), cs) = memo findCombinations'' (ss, cs)
findCombinations'' (ps@('#' : _), cs@(c : cc)) = if canFillGroup ps c then memo findCombinations'' (drop (c + 1) ps, cc) else return 0
findCombinations'' (ps@('?' : ss), cs@(c : cc)) = do
  ns <- memo findCombinations'' (ss, cs)
  rv <- memo findCombinations'' (drop (c + 1) ps, cc)
  if canFillGroup ps c then return (ns + rv) else return ns

canFillGroup :: String -> Int -> Bool
canFillGroup ps c = c <= length ps && '.' `notElem` take c ps && (c == length ps || ps !! c /= '#')

parseLine :: String -> (String, [Int])
parseLine l = (s, f)
  where
    [s, c] = words l
    f = map read $ splitOn "," c
