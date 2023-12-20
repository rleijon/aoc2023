module Day10 (run) where

import qualified Data.Map.Strict as Map
import Debug.Trace (trace)

type Point = (Int, Int)

type Direction = (Int, Int)

data State = State
  { previousPositions :: [Point],
    startingPoint :: Point,
    direction :: Direction,
    allPoints :: Map.Map Point Char
  }
  deriving (Show)

run :: IO ()
run = do
  ls <- parseLines . lines <$> readFile "src/day10.txt"
  replacement <- head <$> readFile "src/day10man.txt"
  let dir = case replacement of
        'F' -> (1, 0)
        'J' -> (-1, 0)
        '7' -> (-1, 0)
        'L' -> (1, 0)
  let startingPoint = fst $ head $ filter (\(p, c) -> c == 'S') ls
  let ls' = Map.fromList $ map (\(p, c) -> if c == 'S' then (p, replacement) else (p, c)) ls
  let loop = getLoop (State [startingPoint] startingPoint dir ls')
  print $ "Day 10, Part 1: " ++ show (length loop `div` 2)
  let v = map fst $ filter (\(b, _) -> b `notElem` loop) (Map.toList ls')
  let pointsInside = filter (\x -> rayCast x loop ls') v
  print pointsInside
  print $ "Day 10, Part 2: " ++ show (length pointsInside)
  print "Done"

rayCast :: Point -> [Point] -> Map.Map Point Char -> Bool
rayCast p@(x, y) loop m = do
  let maxx = maximum $ map (fst . fst) (Map.toList m)
  let xppts = countWallsV [v | xv <- [x .. maxx], (xv, y) `elem` loop, v <- [m Map.! (xv, y)], v /= '-']
  odd xppts

countWallsH :: [Char] -> Int
countWallsH [] = 0
countWallsH ('-' : xs) = 1 + countWallsH xs
countWallsH (v : z : xs) = do
  case (v, z) of
    ('L', '7') -> 1 + countWallsH xs
    ('7', 'L') -> 1 + countWallsH xs
    ('L', 'F') -> 0 + countWallsH xs
    ('F', 'L') -> 0 + countWallsH xs
    ('F', 'J') -> 1 + countWallsH xs
    ('J', 'F') -> 1 + countWallsH xs
    ('J', '7') -> 0 + countWallsH xs
    ('7', 'J') -> 0 + countWallsH xs
    _ -> error (show [v, z])

countWallsV :: [Char] -> Int
countWallsV [] = 0
countWallsV ('|' : xs) = 1 + countWallsV xs
countWallsV (v : z : xs) = do
  case (v, z) of
    ('L', 'J') -> 0 + countWallsV xs
    ('J', 'L') -> 0 + countWallsV xs
    ('L', '7') -> 1 + countWallsV xs
    ('7', 'L') -> 1 + countWallsV xs
    ('F', 'J') -> 1 + countWallsV xs
    ('J', 'F') -> 1 + countWallsV xs
    ('F', '7') -> 0 + countWallsV xs
    ('7', 'F') -> 0 + countWallsV xs
    _ -> error (show [v, z])

getLoop :: State -> [Point]
getLoop s@(State p@(prev : ps) sp d m)
  | sp == prev && not (null ps) = ps
  | otherwise = getLoop (State (np : p) sp nd m)
  where
    (np, nd) = nextPoint p d m

nextPoint :: [Point] -> Point -> Map.Map Point Char -> (Point, Direction)
nextPoint ((x, y) : ps) dir@(dx, dy) m = case (m Map.! nd, dir) of
  ('F', (0, -1)) -> (nd, (1, 0))
  ('F', (-1, 0)) -> (nd, (0, 1))
  ('J', (0, 1)) -> (nd, (-1, 0))
  ('J', (1, 0)) -> (nd, (0, -1))
  ('L', (0, 1)) -> (nd, (1, 0))
  ('L', (-1, 0)) -> (nd, (0, -1))
  ('7', (0, -1)) -> (nd, (-1, 0))
  ('7', (1, 0)) -> (nd, (0, 1))
  ('|', _) -> (nd, dir)
  ('-', _) -> (nd, dir)
  (x, v) -> error $ show (x, v)
  where
    nd = (x + dx, y + dy)

parseLines :: [String] -> [(Point, Char)]
parseLines lines = concatMap parseLine (zip [0 ..] lines)

parseLine :: (Int, String) -> [(Point, Char)]
parseLine (y, str) = zip [(x, y) | x <- [0 .. length str - 1]] str
