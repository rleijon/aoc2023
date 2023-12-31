{-# LANGUAGE BangPatterns #-}

module Day25 where

import Control.Monad.State
import Data.Function (on)
import Data.Graph (Graph, components, graphFromEdges)
import Data.Heap (Heap, MinHeap, MinPrioHeap)
import Data.List (minimumBy, sortBy)
import Data.List.Split (splitOn)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as Set
import Debug.Trace (trace)
import HGraph.Undirected (Mutable (removeEdge))
import System.Random (StdGen, mkStdGen, randomR, setStdGen)

type Vertex' = String

type Edge' = (Vertex', Vertex')

type Path = [Edge']

run :: IO ()
run = do
  (edges, vertices) <- parseLines [] . lines <$> readFile "src/day25.txt"
  let vs = Set.toList vertices
  let ds = dijkstras100 (take 100 vs) vs edges Map.empty
  let re = map fst $ take 6 $ sortBy (flip compare `on` snd) (Map.toList ds)
  let s = Set.toList $ Set.filter (`notElem` re) edges
  let g = buildGraph (Map.fromList $ zip [1 ..] s, vertices)
  print $ "Day 25, Part 1: " ++ show (product $ map length $ components g)
  print "Done"

countEdges :: [(Vertex', Vertex')] -> Map.Map Edge' Int -> Map.Map Edge' Int
countEdges [] m = m
countEdges ((f, t) : xs) m = countEdges xs (Map.insertWith (+) (f, t) 1 (Map.insertWith (+) (t, f) 1 m))

dijkstras100 :: [Vertex'] -> [Vertex'] -> Set.Set Edge' -> Map.Map Edge' Int -> Map.Map Edge' Int
dijkstras100 [] s e m = m
dijkstras100 (x : xs) s e m = do
  let v = evalState (dijkstras s e) (Map.singleton x 0, Map.empty)
  let nc = countEdges (Map.toList v) m
  dijkstras100 xs s e nc

dijkstras :: [Vertex'] -> Set.Set Edge' -> State (Map.Map Vertex' Int, Map.Map Vertex' Vertex') (Map.Map Vertex' Vertex')
dijkstras [] es = do
  (_, v) <- get
  return v
dijkstras q es = do
  (d', _) <- get
  let (u, du) = minimumBy (compare `on` snd) $ map (\x -> (x, fromMaybe 999999 (Map.lookup x d'))) q
  let q' = filter (/= u) q
  forM_
    [t | (f, t) <- Set.toList es, f == u, t `elem` q']
    ( \x -> do
        (d, pr) <- get
        let alt = du + 1
        let dv = fromMaybe 999999 (Map.lookup x d)
        when (alt < dv) $ put (Map.insert x alt d, Map.insert x u pr)
    )
  dijkstras q' es

removeThreeRandomEdges :: StdGen -> Map.Map Int Edge' -> (StdGen, Map.Map Int Edge')
removeThreeRandomEdges s es = do
  let ((f1, t1), s1) = getRandom s es
  let ((f2, t2), s2) = getRandom s1 es
  let ((f3, t3), s3) = getRandom s2 es
  (s3, Map.filter (\e -> e `notElem` [(f1, t1), (t1, f1), (f2, t2), (t2, f2), (f3, t3), (t3, f3)]) es)

getRandomVertex :: StdGen -> Set.Set Vertex' -> (Vertex', StdGen)
getRandomVertex s e = do
  let (rand, s') = randomR (0, Set.size e - 1) s
  (Set.toList e !! rand, s')

getRandom :: StdGen -> Map.Map Int Edge' -> (Edge', StdGen)
getRandom s e = do
  let (rand, s') = randomR (0, Map.size e - 1) s
  (e Map.! rand, s')

buildGraph :: (Map.Map Int Edge', Set.Set Vertex') -> Graph
buildGraph (es, vs) = do
  let a = [(v, v, [t | (i, (f, t)) <- Map.toList es, f == v]) | v <- Set.toList vs]
  let (g, _, _) = graphFromEdges a
  g

parseLines :: [Edge'] -> [String] -> (Set.Set Edge', Set.Set Vertex')
parseLines t [] = (Set.fromList t, Set.union (Set.fromList (map fst t)) (Set.fromList (map snd t)))
parseLines t (x : xs) = do
  let [a, b] = splitOn ": " x
  let ns = concat [[(a, w), (w, a)] | w <- words b]
  parseLines (ns ++ t) xs