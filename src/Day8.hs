module Day8 (run) where

import Data.Function (on)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import GHC.RTS.Flags (DebugFlags (gc))

type LR = (String, String)

run :: IO ()
run = do
  (instructions, network) <- parseNetwork <$> readFile "src/day8.txt"
  print $ "Day 8, Part 1: " ++ show (reachZZZ 0 "AAA" (cycle instructions) network)
  let v = [reachZ 0 i (cycle instructions) network | i <- Map.keys network, last i == 'A']
  let lcm' = foldl lcm 1 v
  print $ "Day 8, Part 2: " ++ show lcm'
  print "Done"

reachZ :: Int -> String -> [Char] -> Map.Map String LR -> Int
reachZ c [_, _, 'Z'] _ _ = c
reachZ c curr (x : xs) m
  | x == 'L' = reachZ (c + 1) (fst (m Map.! curr)) xs m
  | x == 'R' = reachZ (c + 1) (snd (m Map.! curr)) xs m

reachZZZ :: Int -> String -> [Char] -> Map.Map String LR -> Int
reachZZZ c "ZZZ" _ _ = c
reachZZZ c curr (x : xs) m
  | x == 'L' = reachZZZ (c + 1) (fst (m Map.! curr)) xs m
  | x == 'R' = reachZZZ (c + 1) (snd (m Map.! curr)) xs m

parseNetwork :: String -> ([Char], Map.Map String LR)
parseNetwork s = do
  let [a, b] = splitOn "\n\n" s
  let ls = lines b
  (a, Map.fromList [(take 3 l, (take 3 (drop 7 l), take 3 (drop 12 l))) | l <- ls])
