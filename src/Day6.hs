module Day6 (run) where

import Data.Char (digitToInt, isDigit)
import Data.List (findIndex, isPrefixOf, isSuffixOf)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)
import Debug.Trace (trace)

run :: IO ()
run = do
  ls <- lines <$> readFile "src/day6.txt"
  let td = parseLines ls
  let sims = map simulate' td
  print $ "Day 6, Part 1: " ++ show (product sims)
  let td' = parseLines' ls
  let sim = simulate' td'
  print $ "Day 6, Part 2: " ++ show sim
  print "Done"

parseLines' :: [String] -> (Int, Int)
parseLines' [x, y] = (read xv :: Int, read yv :: Int)
  where
    xv = concat $ tail $ words x
    yv = concat $ tail $ words y

simulate' :: (Int, Int) -> Int
simulate' (t, d) = let v = [holdTime | holdTime <- [1 .. t - 1], nd <- [holdTime * (t - holdTime)], d < nd] in length v

parseLines :: [String] -> [(Int, Int)]
parseLines [x, y] = zip times dists
  where
    times = map (\x -> read x :: Int) (words $ drop 9 x)
    dists = map (\x -> read x :: Int) (words $ drop 9 y)