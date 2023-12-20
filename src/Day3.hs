module Day3 (run) where

import Data.Char (isDigit)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust, isJust)

type Point = (Int, Int)

run :: IO ()
run = do
  ls <- lines <$> readFile "src/day3.txt"
  let k = parseLines ls
  let v = parseLines' ls
  let allNumbers = filterInts k v
  let allGears = filterGears k v
  print $ "Day 3, Part 1: " ++ show (sum allNumbers)
  print $ "Day 3, Part 2: " ++ show (sum allGears)
  print "Done"

filterGears :: [(Point, Char)] -> [(Int, [Point])] -> [Int]
filterGears mc mi = do
  let vs = map fst $ filter (\(a, b) -> b == '*') mc
  let nearGears = [catMaybes $ lookupInts [(x + xd, y + yd) | xd <- [-1 .. 1], yd <- [-1 .. 1]] mi | (x, y) <- vs]
  map product $ filter (\l -> length l > 1) nearGears

lookupInts :: [(Int, Int)] -> [(Int, [Point])] -> [Maybe Int]
lookupInts [] m = []
lookupInts (p : xs) m = v : lookupInts xs newM
  where
    v = if null ftd then Nothing else Just (fst $ head ftd)
    ftd = filter (\(i, pts) -> p `elem` pts) m
    newM = filter (\(i, pts) -> p `notElem` pts) m

filterInts :: [(Point, Char)] -> [(Int, [Point])] -> [Int]
filterInts mc mi = do
  let vs = map fst $ filter (\(a, b) -> b /= '.' && not (isDigit b)) mc
  let allPoints = [(x + xd, y + yd) | (x, y) <- vs, xd <- [-1 .. 1], yd <- [-1 .. 1]]
  let filteredPts = filter (\(i, pts) -> or [p `elem` allPoints | p <- pts]) mi
  map fst filteredPts

parseLines :: [String] -> [(Point, Char)]
parseLines s = concatMap parseLine (zip [0 ..] s)

parseLine :: (Int, String) -> [(Point, Char)]
parseLine (y, s) = zipWith (\x c -> ((x, y), c)) [0 ..] s

parseLines' :: [String] -> [(Int, [Point])]
parseLines' s = concatMap (\l -> parseLine' (parseLine l) [] []) (zip [0 ..] s)
  where
    parseLine' [] [] v = v
    parseLine' [] cs v = asInt cs : v
    parseLine' ((p, c) : xs) cs v
      | c == '.' && null cs = parseLine' xs [] v
      | isDigit c = parseLine' xs (cs ++ [(p, c)]) v
      | otherwise = parseLine' xs [] (asInt cs : v)

asInt :: [(Point, Char)] -> (Int, [Point])
asInt ps = (read (map snd ps) :: Int, map fst ps)
