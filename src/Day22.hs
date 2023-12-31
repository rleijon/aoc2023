{-# LANGUAGE InstanceSigs #-}

module Day22 where

import Control.Monad.State
import qualified Data.Bifunctor
import Data.Function (on)
import Data.List (sort, sortBy)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Trace (trace)

type Point = (Int, Int, Int)

data Brick = Brick
  { name :: Int,
    coords :: Set.Set Point,
    standing :: Bool
  }
  deriving (Show, Eq)

instance Ord Brick where
  compare :: Brick -> Brick -> Ordering
  compare (Brick _ c1 _) (Brick _ c2 _) = do
    let a = minimum $ Set.map (\(_, _, z) -> z) c1
    let b = minimum $ Set.map (\(_, _, z) -> z) c2
    compare a b

run :: IO ()
run = do
  ls <- sort . zipWith (curry toBrick) [1 ..] . map parseLine . lines <$> readFile "src/day22.txt"
  let m = moveBrick ls []
  let p1 = length $ filter id [canDisintegrate b m | b <- sortBy (compare `on` name) m]
  print $ "Day 22, Part 1: " ++ show p1
  let p2 = sum [countDisintegrate b m | b <- sortBy (compare `on` name) m]
  print $ "Day 22, Part 2: " ++ show p2
  print "Done"

countDisintegrate :: Brick -> [Brick] -> Int
countDisintegrate (Brick n c s) bs = do
  let a = sortBy (compare `on` name) $ filter (\(Brick n' c' s') -> n' /= n) bs
  let a' = sortBy (compare `on` name) $ moveBrick (sort a) []
  countDifferences a a'

countDifferences :: [Brick] -> [Brick] -> Int
countDifferences [] [] = 0
countDifferences (x : bs) (x' : bs') = v + countDifferences bs bs'
  where
    v = if x == x' then 0 else 1

canDisintegrate :: Brick -> [Brick] -> Bool
canDisintegrate (Brick n c s) bs = do
  let a = sortBy (compare `on` name) $ filter (\(Brick n' c' s') -> n' /= n) bs
  let a' = sortBy (compare `on` name) $ moveBrick a []
  a == a'

moveBrick :: [Brick] -> [Brick] -> [Brick]
moveBrick [] b = b
moveBrick (br@(Brick n c s) : bs) b = do
  let newc = Set.map (\(x, y, z) -> (x, y, z - 1)) c
  let isBlocked = any (\p@(_, _, z) -> or [Set.member p br' | (Brick _ br' _) <- bs] || or [Set.member p br' | (Brick _ br' _) <- b] || z == 0) newc
  if isBlocked
    then moveBrick bs (br : b)
    else moveBrick (Brick n newc s : bs) b

toBrick :: (Int, [Point]) -> Brick
toBrick (n, c) = Brick n (Set.fromList c) s
  where
    s = null $ Set.fromList (map (\(x, y, z) -> z) c)

parseLine :: String -> [Point]
parseLine s = do
  let [f, t] = splitOn "~" s
  let [x, y, z] = map (\x -> read x :: Int) $ splitOn "," f
  let [x', y', z'] = map (\x -> read x :: Int) $ splitOn "," t
  let pts
        | x > x' = [(x'', y, z) | x'' <- [x' .. x]]
        | x < x' = [(x'', y, z) | x'' <- [x .. x']]
        | y > y' = [(x, y'', z) | y'' <- [y' .. y]]
        | y < y' = [(x, y'', z) | y'' <- [y .. y']]
        | z > z' = [(x, y, z'') | z'' <- [z' .. z]]
        | z < z' = [(x, y, z'') | z'' <- [z .. z']]
        | otherwise = [(x, y, z)]
  pts