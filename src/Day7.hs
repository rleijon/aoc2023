module Day7 (run) where

import Data.Char (digitToInt, isDigit)
import Data.Function (on)
import Data.List (findIndex, isPrefixOf, isSuffixOf, sortBy)
import Data.List.Split (splitOn)
import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust)

data HandType = FiveOfAKind | FourOfAKind | FullHouse | ThreeOfAKind | TwoPairs | OnePair | HighCard
  deriving (Ord, Eq, Show)

data Hand = Hand
  { cards :: [Char],
    count :: Map.Map Char Int,
    handType :: HandType,
    bid :: Int
  }
  deriving (Show)

run :: IO ()
run = do
  hands <- map parseHand . lines <$> readFile "src/day7.txt"
  let winning = sortBy orderHands hands
  let ls = zip [1 ..] $ map bid winning
  print $ "Day 7, Part 1: " ++ show (sum $ map (uncurry (*)) ls)
  handsJ <- map parseHand' . lines <$> readFile "src/day7.txt"
  let winningJ = sortBy orderHands' handsJ
  let lsJ = zip [1 ..] $ map bid winningJ
  print $ "Day 7, Part 2: " ++ show (sum $ map (uncurry (*)) lsJ)
  print "Done"

orderHands' :: Hand -> Hand -> Ordering
orderHands' h1 h2 = case compare (handType h1) (handType h2) of
  EQ -> compareCards' (cards h1) (cards h2)
  LT -> GT
  GT -> LT

orderHands :: Hand -> Hand -> Ordering
orderHands h1 h2 = case compare (handType h1) (handType h2) of
  EQ -> compareCards (cards h1) (cards h2)
  LT -> GT
  GT -> LT

compareCards' :: [Char] -> [Char] -> Ordering
compareCards' [] [] = EQ
compareCards' (x : xs) (y : ys)
  | x == y = compareCards' xs ys
  | x == 'J' = LT
  | y == 'J' = GT
  | x == 'A' = GT
  | y == 'A' = LT
  | x == 'K' = GT
  | y == 'K' = LT
  | x == 'Q' = GT
  | y == 'Q' = LT
  | otherwise = compare x y

compareCards :: [Char] -> [Char] -> Ordering
compareCards [] [] = EQ
compareCards (x : xs) (y : ys)
  | x == y = compareCards xs ys
  | x == 'A' = GT
  | y == 'A' = LT
  | x == 'K' = GT
  | y == 'K' = LT
  | x == 'Q' = GT
  | y == 'Q' = LT
  | x == 'J' = GT
  | y == 'J' = LT
  | otherwise = compare x y

parseHand' :: String -> Hand
parseHand' ls = do
  let [cards, bid] = words ls
  let noJokers = length $ filter (== 'J') cards
  let m = Map.fromListWith (+) [(c, 1) | c <- cards, c /= 'J']
  let t = case (Map.size m, noJokers) of
        (5, 0) -> HighCard
        (4, 0) -> OnePair
        (4, 1) -> OnePair
        (3, 2) -> ThreeOfAKind
        (3, 1) -> ThreeOfAKind
        (3, 0) -> if maximum (Map.elems m) == 3 then ThreeOfAKind else TwoPairs
        (2, 3) -> FourOfAKind
        (2, 2) -> FourOfAKind
        (2, 1) -> if maximum (Map.elems m) == 3 then FourOfAKind else FullHouse
        (2, 0) -> if maximum (Map.elems m) == 4 then FourOfAKind else FullHouse
        (1, _) -> FiveOfAKind
        (0, _) -> FiveOfAKind
        v -> error (show (Map.size m, noJokers))
  Hand cards m t (read bid)

parseHand :: String -> Hand
parseHand ln = do
  let [cards, bid] = words ln
  let m = Map.fromListWith (+) [(c, 1) | c <- cards]
  let t = case Map.size m of
        5 -> HighCard
        4 -> OnePair
        3 -> if maximum (Map.elems m) == 3 then ThreeOfAKind else TwoPairs
        2 -> if maximum (Map.elems m) == 4 then FourOfAKind else FullHouse
        1 -> FiveOfAKind
  Hand cards m t (read bid)