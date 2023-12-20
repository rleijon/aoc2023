module Day5 (run) where

import Data.List (sort)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Debug.Trace (trace)

type Range = (Int, Int, Int)

run :: IO ()
run = do
  games <- splitOn "\n\n" <$> readFile "src/day5.txt"
  let seeds = map (\r -> read r :: Int) $ tail (words $ head games)
  let mappings = map parseMap (tail games)
  print $ "Day 5, Part 1: " ++ show (minimum [lookupSeed s mappings | s <- seeds])
  let v = [process [sr] mappings | sr <- seedRanges seeds]
  print $ "Day 5, Part 2: " ++ show (fst $ minimum v)
  print "Done"

process :: [(Int, Int)] -> [[Range]] -> (Int, Int)
process seed [] = minimum seed
process seed (r : rs) = process (concatMap mr seed) rs
  where
    mr v = let rs = mapRange (sort r) v in if null rs then [v] else rs

mapRange :: [Range] -> (Int, Int) -> [(Int, Int)]
mapRange [] (s, e) = []
mapRange ((s', e', d) : xs) (s, e)
  | s > e = []
  | s > e' || e < s' = mapRange xs (s, e)
  | s < s' = [(s, s' - 1), (s' + d, min e e' + d)] ++ mapRange xs (e', e)
  | otherwise = (s + d, min e e' + d) : mapRange xs (e', e)

lookupSeed :: Int -> [[Range]] -> Int
lookupSeed v [] = v
lookupSeed v (x : xs) = case lookupInRanges v x of
  Just n -> lookupSeed n xs
  Nothing -> lookupSeed v xs

lookupInRanges :: Int -> [Range] -> Maybe Int
lookupInRanges v [] = Nothing
lookupInRanges v ((s, e, d) : xs) = if v >= s && v <= e then Just (v + d) else lookupInRanges v xs

parseMap :: String -> [Range]
parseMap v = [parseMap' ln | ln <- tail (lines v)]

parseMap' :: String -> Range
parseMap' ln = (srcStart, srcStart + range - 1, destStart - srcStart)
  where
    [dest, src, rng] = words ln
    destStart = read dest :: Int
    srcStart = read src :: Int
    range = read rng :: Int

seedRanges :: [Int] -> [(Int, Int)]
seedRanges [] = []
seedRanges [x] = error "Unexpected"
seedRanges (x : y : xs) = (x, x + y) : seedRanges xs
