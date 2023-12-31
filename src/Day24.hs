module Day24 where

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromJust)
import Debug.Trace (trace)
import Math.LinearEquationSolver (Solver (Z3), solveRationalLinearEqs)

type Point = (Rational, Rational, Rational)

type Velocity = (Rational, Rational, Rational)

run :: IO ()
run = do
  ls <- zip [1 ..] . map parseLine . lines <$> readFile "src/day24.txt"
  [mn, mx] <- map (toRational . (\x -> read x :: Int)) . splitOn "," <$> readFile "src/day24ta.txt"
  let as = catMaybes [overlapsXY h h' mn mx | (i, h) <- ls, (j, h') <- ls, i < j]
  let m = Map.fromList ls
  let cm = getCoefficientMatrix m
  let rs = getResultVector m
  s <- solveRationalLinearEqs Z3 cm rs
  print $ "Day 24, Part 1: " ++ show (length as)
  print $ "Day 24, Part 2: " ++ show (sum $ fromJust s)
  print "Done"

getResultVector :: Map.Map Integer (Point, Velocity) -> [Rational]
getResultVector m = do
  let h0 = m Map.! 1
  let h1 = m Map.! 2
  let h2 = m Map.! 3
  [-getResult h0 h1, -getResult h1 h2, -getResult h0 h2]

getCoefficientMatrix :: Map.Map Integer (Point, Velocity) -> [[Rational]]
getCoefficientMatrix m = do
  let h0 = m Map.! 1
  let h1 = m Map.! 2
  let h2 = m Map.! 3
  [getCoefficients h0 h1, getCoefficients h1 h2, getCoefficients h0 h2]

getResult :: (Point, Velocity) -> (Point, Velocity) -> Rational
getResult ((pxi, pyi, pzi), (vxi, vyi, vzi)) ((pxj, pyj, pzj), (vxj, vyj, vzj)) = do
  -- pi ^ vi ^ pj + pj ^ vj ^ pi
  let a = pxi * vyi * pzj + pyi * vzi * pxj + pzi * vxi * pyj
  let b = pxi * vzi * pyj + pyi * vxi * pzj + pzi * vyi * pxj
  let a' = pxj * vyj * pzi + pyj * vzj * pxi + pzj * vxj * pyi
  let b' = pxj * vzj * pyi + pyj * vxj * pzi + pzj * vyj * pxi
  a - b + a' - b'

getCoefficients :: (Point, Velocity) -> (Point, Velocity) -> [Rational]
getCoefficients (p1, v1) (p2, v2) = do
  let (px, py, pz) = p1 `minus` p2
  let (vx, vy, vz) = v1 `minus` v2
  let a = vy * pz - vz * py
  let b = vz * px - vx * pz
  let c = vx * py - vy * px
  [a, b, c]

minus :: (Rational, Rational, Rational) -> (Rational, Rational, Rational) -> (Rational, Rational, Rational)
minus (x, y, z) (x', y', z') = (x - x', y - y', z - z')

overlapsXY :: (Point, Velocity) -> (Point, Velocity) -> Rational -> Rational -> Maybe Point
overlapsXY ((x, y, _), (dx, dy, _)) ((x', y', _), (dx', dy', _)) mn mx = do
  if ((dx' * dy / dy') - dx) == 0
    then Nothing -- parallel
    else do
      let t = ((dx' / dy') * (y' - y) + (x - x')) / ((dx' * dy / dy') - dx)
      let t' = ((x - x') + dx * t) / dx'
      let xc = x + dx * t
      let yc = y + dy * t
      if t < 0 || t' < 0 || xc < mn || xc > mx || yc < mn || yc > mx
        then Nothing
        else Just (xc, yc, 0)

parseLine :: String -> (Point, Velocity)
parseLine s = do
  let [p, d] = splitOn " @ " s
  let [x, y, z] = map (toRational . (\x -> read x :: Int)) (splitOn ", " p)
  let [dx, dy, dz] = map (toRational . (\x -> read x :: Int)) $ splitOn ", " d
  ((x, y, z), (dx, dy, dz))
