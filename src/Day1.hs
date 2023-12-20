module Day1 (run) where

import Control.Concurrent (waitQSem)
import Data.Char (digitToInt, isDigit)
import Data.List (findIndex, isPrefixOf, isSuffixOf)
import Data.Maybe (fromJust, isJust, isNothing)
import Debug.Trace (trace)
import Util (findLastIndex)

run :: IO ()
run = do
  ls <- lines <$> readFile "src/day1.txt"
  let frequencies = map readVal ls
  let part2 = map readVal' ls
  let v = [read (show (head l) ++ show (last l)) :: Int | l <- frequencies]
  let v2 = [read (show (head l) ++ show (last l)) :: Int | l <- part2]
  print $ "Day 1, Part 1: " ++ show (sum v)
  print $ "Day 1, Part 2: " ++ show (sum v2)
  print "Done"

readVal' :: String -> [Int]
readVal' s = do
  let fi = findIndex isDigit s
  let fsv = findSubstringIndex s
  let li = findLastIndex isDigit s
  let lsv = findSubstringIndexR s
  let fv = if isJust fi && (isNothing fsv || fromJust fi < snd (fromJust fsv)) then digitToInt $ head (filter isDigit s) else fst (fromJust fsv)
  let lv = if isJust li && (isNothing lsv || fromJust li > snd (fromJust lsv)) then digitToInt $ last (filter isDigit s) else fst (fromJust lsv)
  [fv, lv]

findSubstringIndexR :: String -> Maybe (Int, Int)
findSubstringIndexR = go 0
  where
    go i s
      | null s = Nothing
      | "zero" `isSuffixOf` s = Just (0, length s - 4)
      | "one" `isSuffixOf` s = Just (1, length s - 3)
      | "two" `isSuffixOf` s = Just (2, length s - 3)
      | "three" `isSuffixOf` s = Just (3, length s - 5)
      | "four" `isSuffixOf` s = Just (4, length s - 4)
      | "five" `isSuffixOf` s = Just (5, length s - 4)
      | "six" `isSuffixOf` s = Just (6, length s - 3)
      | "seven" `isSuffixOf` s = Just (7, length s - 5)
      | "eight" `isSuffixOf` s = Just (8, length s - 5)
      | "nine" `isSuffixOf` s = Just (9, length s - 4)
      | otherwise = go (i + 1) (init s)

findSubstringIndex :: String -> Maybe (Int, Int)
findSubstringIndex s = go 0 s
  where
    go i s
      | "zero" `isPrefixOf` s = Just (0, i)
      | "one" `isPrefixOf` s = Just (1, i)
      | "two" `isPrefixOf` s = Just (2, i)
      | "three" `isPrefixOf` s = Just (3, i)
      | "four" `isPrefixOf` s = Just (4, i)
      | "five" `isPrefixOf` s = Just (5, i)
      | "six" `isPrefixOf` s = Just (6, i)
      | "seven" `isPrefixOf` s = Just (7, i)
      | "eight" `isPrefixOf` s = Just (8, i)
      | "nine" `isPrefixOf` s = Just (9, i)
      | null s = Nothing
      | otherwise = go (i + 1) (tail s)

readVal :: [Char] -> [Int]
readVal s = [digitToInt c | c <- s, isDigit c]