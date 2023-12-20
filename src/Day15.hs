module Day15 (run) where

import Control.Monad.State
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Vector.Generic (new)

data Operation = REMOVE String | REPLACE String Int deriving (Show, Eq)

type Boxes = Map.Map Int [(String, Int)]

run :: IO ()
run = do
  ls <- splitOn "," <$> readFile "src/day15.txt"
  let v = map (`hash` 0) ls
  print $ "Day 15, Part 1: " ++ show (sum v)
  let vs = map toOperation ls
  let rs = evalState (runOperation vs) (Map.fromList [(h, []) | h <- [0 .. 255]])
  print $ "Day 15, Part 2: " ++ show (calculateFocusingPower rs)
  print "Done"

calculateFocusingPower :: Boxes -> Int
calculateFocusingPower boxes = sum $ map calculateBoxPower $ Map.toList boxes
  where
    calculateBoxPower (i, bs) = sum $ zipWith (curry (\(s, (_, c)) -> s * c * (i + 1))) [1 ..] (reverse bs)

runOperation :: [Operation] -> State Boxes Boxes
runOperation [] = get
runOperation (REMOVE v : xs) = do
  boxes <- get
  let h = hash v 0
  let boxes' = filter (\(s, i) -> s /= v) (boxes Map.! h)
  put $ Map.insert h boxes' boxes
  runOperation xs
runOperation (REPLACE v i : xs) = do
  boxes <- get
  let h = hash v 0
  let boxes' = boxes Map.! h
  let newBoxes =
        if not (any (\(s, c) -> s == v) boxes')
          then (v, i) : boxes'
          else map (\(s, x) -> if s == v then (s, i) else (s, x)) boxes'
  put $ Map.insert h newBoxes boxes
  runOperation xs

toOperation :: String -> Operation
toOperation s
  | last s == '-' = REMOVE (init s)
  | otherwise = REPLACE (init $ init s) (read [last s] :: Int)

hash :: String -> Int -> Int
hash [] i = i
hash (x : xs) i = hash xs ((i + fromEnum x) * 17) `mod` 256
