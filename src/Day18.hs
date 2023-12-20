module Day18 (run) where

import qualified Data.Set as Set
import Numeric (readHex)

type Point = (Int, Int)

type Direction = (Int, Int)

run :: IO ()
run = do
  plans <- map parseLine . lines <$> readFile "src/day18.txt"
  let sz = getLength plans
  let vertices = followPlan' (0, 0) plans []
  print $ "Day 18, Part 1: " ++ show ((sz `div` 2) + shoelace (reverse vertices) + 1)
  plans' <- map parseLine' . lines <$> readFile "src/day18.txt"
  let sz' = getLength plans'
  let vertices' = followPlan' (0, 0) plans' []
  print $ "Day 18, Part 2: " ++ show ((sz' `div` 2) + shoelace (reverse vertices') + 1)
  print "Done"

shoelace :: [Point] -> Int
shoelace pts@(p : ps) = do
  let a = pts ++ [p]
  sum
    [ let (xi, yi) = a !! i
          (xi1, yi1) = a !! (i + 1)
       in (yi + yi1) * (xi - xi1)
      | i <- [0 .. length pts - 1]
    ]
    `div` 2

getLength :: [(Direction, Int, String)] -> Int
getLength [] = 0
getLength (((dx, dy), l, _) : xs) = l + getLength xs

followPlan' :: Point -> [(Direction, Int, String)] -> [Point] -> [Point]
followPlan' _ [] s = s
followPlan' (x, y) (((dx, dy), l, _) : xs) s = do
  let np = (x + l * dx, y + l * dy)
  followPlan' np xs (np : s)

parseLine' :: String -> (Direction, Int, String)
parseLine' s = do
  let [_, _, c] = words s
  let hex = drop 2 (init c)
  let d = case last hex of
        '0' -> (1, 0)
        '1' -> (0, 1)
        '2' -> (-1, 0)
        '3' -> (0, -1)
  (d, read $ "0x" ++ take 5 hex, c)

parseLine :: String -> (Direction, Int, String)
parseLine s = do
  let [a, b, c] = words s
  let d = case a of
        "R" -> (1, 0)
        "L" -> (-1, 0)
        "D" -> (0, 1)
        "U" -> (0, -1)
  (d, read b :: Int, c)
