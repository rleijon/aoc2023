{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use tuple-section" #-}
module Day20 where

import Control.Monad.State
import Data.List.Split (splitOn)
import Data.Map ((!))
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromJust, isJust, mapMaybe)
import Debug.Trace (trace)
import GHC.TypeLits (Mod)

data ModuleType = BDX | FLP | CON deriving (Show, Eq)

type Module = (ModuleType, [String])

data Pulse = LOW | HIGH deriving (Show, Eq)

data Broadcast = Broadcast
  { pulse :: Pulse,
    from :: String,
    to :: String
  }
  deriving (Show, Eq)

type ConState = Map.Map String Pulse

data ModuleStates = ModuleStates
  { conStates :: Map.Map String ConState,
    flipStates :: Map.Map String Bool
  }

run :: IO ()
run = do
  ls' <- Map.fromList . map parseLine . lines <$> readFile "src/day20.txt"
  let ls = decorateWithDummies ls'
  let r = initialState ls
  let (l, h) = evaluateNTimes 1000 ls r (0, 0)
  print $ "Day 20, Part 1: " ++ show (l * h)
  let f = head [l | (l, (v, e)) <- Map.toList ls, "rx" `elem` e]
  let deps = Map.fromList [(l, Nothing) | (l, (v, e)) <- Map.toList ls, f `elem` e]
  let v = evalState (evaluateUntil' 1 ls r) deps
  print $ "Day 20, Part 2: " ++ show v
  print "Done"

decorateWithDummies :: Map.Map String Module -> Map.Map String Module
decorateWithDummies m = do
  let v = map (\x -> (x, (BDX, []))) $ concatMap (filter (`Map.notMember` m) . snd) (Map.elems m)
  Map.union m (Map.fromList v)

evaluateUntil' :: Int -> Map.Map String Module -> ModuleStates -> State (Map.Map String (Maybe Int)) Int
evaluateUntil' n mds s = do
  let v = evaluate' [Broadcast LOW "button" "broadcaster"] mds
  let (a, b) = evalState v (s, [])
  m <- get
  let u =
        [ case (any (\(Broadcast p f t) -> p == HIGH && f == k) b, v) of
            (True, Nothing) -> (k, Just n)
            _ -> (k, v)
          | (k, v) <- Map.toList m
        ]
  put $ Map.fromList u
  if all (\(k, v) -> isJust v) u
    then return $ foldl lcm 1 (map (fromJust . snd) u)
    else evaluateUntil' (n + 1) mds a

evaluateNTimes :: Int -> Map.Map String Module -> ModuleStates -> (Int, Int) -> (Int, Int)
evaluateNTimes 0 mds s bdx = bdx
evaluateNTimes n mds s (low, high) = do
  let v = evaluate' [Broadcast LOW "button" "broadcaster"] mds
  let (a, b) = evalState v (s, [])
  let ls = length $ filter (== LOW) $ map pulse b
  let hs = length $ filter (== HIGH) $ map pulse b
  evaluateNTimes (n - 1) mds a (low + ls, hs + high)

evaluate' :: [Broadcast] -> Map.Map String Module -> State (ModuleStates, [Broadcast]) (ModuleStates, [Broadcast])
evaluate' [] _ = get
evaluate' (b@(Broadcast p f t) : xs) m = do
  let (tp, outputs) = m ! t
  (md@(ModuleStates cs fs), bdx) <- get
  let bdx' = b : bdx
  r <- case (tp, p) of
    (BDX, _) -> do
      put (md, bdx')
      return [Broadcast p t o | o <- outputs]
    (FLP, HIGH) -> do
      put (md, bdx')
      return []
    (FLP, LOW) -> do
      let f = fs ! t
      put (ModuleStates cs (Map.insert t (not f) fs), bdx')
      if f
        then return [Broadcast LOW t o | o <- outputs]
        else return [Broadcast HIGH t o | o <- outputs]
    (CON, _) -> do
      let c' = Map.insert f p (cs ! t)
      put (ModuleStates (Map.insert t c' cs) fs, bdx')
      if all (== HIGH) $ Map.elems c'
        then return [Broadcast LOW t o | o <- outputs]
        else return [Broadcast HIGH t o | o <- outputs]
  evaluate' (xs ++ r) m

initialState :: Map.Map String Module -> ModuleStates
initialState ms = do
  let flipStates = Map.map (const False) $ Map.filter (\(t, _) -> t == FLP) ms
  let conStates = Map.mapWithKey (`getAllInputs` ms) $ Map.filter (\(t, _) -> t == CON) ms
  ModuleStates conStates flipStates

getAllInputs :: String -> Map.Map String Module -> Module -> Map.Map String Pulse
getAllInputs mod mp _ = Map.fromList $ mapMaybe (\(k, (_, o)) -> if mod `elem` o then Just (k, LOW) else Nothing) (Map.toList mp)

parseLine :: String -> (String, Module)
parseLine line = case head line of
  '%' -> (name, (FLP, output))
  'b' -> ("broadcaster", (BDX, output))
  '&' -> (name, (CON, output))
  _ -> error line
  where
    [n, s] = splitOn " -> " line
    name = tail n
    output = splitOn ", " s
