module Day13 (run) where

import Data.List (transpose)
import Data.List.Split (splitOn)
import Debug.Trace (trace)

run :: IO ()
run = do
  f <- readFile "src/day13.txt"
  let patterns = map lines $ splitOn "\n\n" f
  let v = map calculateValue patterns
  print $ "Day 13, Part 1: " ++ show (sum v)
  let v' = map calculateValue' patterns
  print $ "Day 13, Part 2: " ++ show (sum v')
  print "Done"

calculateValue :: [String] -> Int
calculateValue rows = 100 * isReflectionPoint 1 rows + isReflectionPoint 1 (transpose rows)

calculateValue' :: [String] -> Int
calculateValue' rows = 100 * isReflectionPoint' 1 rows + isReflectionPoint' 1 (transpose rows)

isReflectionPoint' :: Int -> [String] -> Int
isReflectionPoint' y rows
  | l == 0 = 0
  | isReflection a' b' == 1 = y
  | otherwise = isReflectionPoint' (y + 1) rows
  where
    (a, b) = splitAt y rows
    l = min (length a) (length b)
    (a', b') = (take l $ reverse a, take l b)

isReflectionPoint :: Int -> [String] -> Int
isReflectionPoint y rows
  | l == 0 = 0
  | isReflection a' b' == 0 = y
  | otherwise = isReflectionPoint (y + 1) rows
  where
    (a, b) = splitAt y rows
    l = min (length a) (length b)
    (a', b') = (take l $ reverse a, take l b)

isReflection :: [String] -> [String] -> Int
isReflection [] [] = 0
isReflection (a : as) (b : bs) =
  if a == b
    then isReflection as bs
    else do
      let mismatches = filter not $ zipWith (==) a b
      length mismatches + isReflection as bs