module Day4 (run) where

import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Debug.Trace (trace)

data Card = Card
  { cardId :: Int,
    winning :: [Int],
    have :: [Int]
  }
  deriving (Show, Eq)

run :: IO ()
run = do
  games <- map parseLine . lines <$> readFile "src/day4.txt"
  print $ "Day 4, Part 1: " ++ show (sum $ map getPoints games)
  let startingCards = Map.fromList $ map (\c -> (cardId c, (1, c))) games
  let cards = calculateCascading 1 startingCards
  let countCards = map (\(i, (c, _)) -> c) (Map.toList cards)
  print $ "Day 4, Part 2: " ++ show (sum countCards)
  print "Done"

calculateCascading :: (Num a) => Int -> Map.Map Int (a, Card) -> Map.Map Int (a, Card)
calculateCascading i holding =
  if i >= length holding
    then holding
    else do
      let (c, Card id w h) = holding Map.! i
      let cardPoints = length (filter (`elem` w) h)
      let newHolding = [(i + v, (c, Card id w h)) | v <- [1 .. cardPoints], i + v <= fst (Map.findMax holding)]
      calculateCascading (i + 1) (Map.unionWith (\(a, b) (c, d) -> (a + c, d)) (Map.fromList newHolding) holding)

getPoints :: Card -> Int
getPoints (Card _ w h) = let l = length (filter (`elem` w) h) in if l == 0 then 0 else 2 ^ (l - 1)

parseLine :: String -> Card
parseLine l = do
  let [a, b] = splitOn ": " l
  let [w, h] = splitOn " | " b
  let ww = map (read :: String -> Int) $ words w
  let hh = map (read :: String -> Int) $ words h
  Card (read (drop 5 a) :: Int) ww hh