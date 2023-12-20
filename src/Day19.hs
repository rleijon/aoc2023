module Day19 where

import Data.List.Split (splitOn)
import Text.Show.Functions

type PartPredicate = Part -> Bool

data Condition = Lit String | Con PartPredicate String deriving (Show)

data Workflow = Workflow String [Condition] deriving (Show)

type Range = (Int, Int)

data Condition' = Lit' String | Filter' (Char, Range) String deriving (Show)

data Workflow' = Workflow' String [Condition'] deriving (Show)

data Part = Part
  { x :: Int,
    m :: Int,
    a :: Int,
    s :: Int
  }
  deriving (Show, Eq)

-- 180394911228524 too high
run :: IO ()
run = do
  (parts, workflows) <- parse <$> readFile "src/day19.txt"
  let accepted = filter (\p -> evaluate p "in" workflows) parts
  let ratings = map getRatings accepted
  print $ "Day 19, Part 1: " ++ show (sum ratings)
  workflows' <- parse' <$> readFile "src/day19.txt"
  -- print (length fs, sum fs)
  -- print evs
  print $ "Day 19, Part 2: " ++ show 2
  print "Done"

getRatings :: Part -> Int
getRatings (Part x' m' a' s') = x' + m' + a' + s'

type Part' = (Range, Range, Range, Range)

evaluate' :: [(String, Part')] -> [Workflow'] -> [Part'] -> [Part']
evaluate' [] wfs pts = pts
evaluate' ((wfn, p) : ps) wfs pts = do
  let (Workflow' _ conds) = head $ filter (\(Workflow' n _) -> n == wfn) wfs
  let nxt = evaluateWorkflow' p conds
  []

evaluateWorkflow' :: Part' -> [Condition'] -> [(String, Part')]
evaluateWorkflow' _ [] = []
evaluateWorkflow' p@((xmin, xmax), m', a', s') (c : cs) = case c of
  Lit' v -> [(v, p)]
  Filter' (c, (rs, re)) v -> []

evaluate :: Part -> String -> [Workflow] -> Bool
evaluate p "A" wfs = True
evaluate p "R" wfs = False
evaluate p wfn wfs = do
  let (Workflow _ conds) = head $ filter (\(Workflow n _) -> n == wfn) wfs
  let nxt = evaluateWorkflow p conds
  evaluate p nxt wfs

evaluateWorkflow :: Part -> [Condition] -> String
evaluateWorkflow p [] = error "Could not find matching condition"
evaluateWorkflow p ((Lit v) : s) = v
evaluateWorkflow p ((Con pp v) : s) = if pp p then v else evaluateWorkflow p s

parse' :: String -> [Workflow']
parse' s = do
  let [w, _] = splitOn "\n\n" s
  let workflows = map parseWorkflow' (lines w)
  workflows

parseWorkflow' :: String -> Workflow'
parseWorkflow' l = Workflow' name (map parseFilter conds)
  where
    [name, rest] = splitOn "{" l
    conds = splitOn "," (init rest)

parseFilter :: String -> Condition'
parseFilter c = case splitOn ":" c of
  [s] -> Lit' s
  [v, s] -> Filter' (parseFilter' v) s

parseFilter' :: [Char] -> (Char, Range)
parseFilter' c =
  let v = read (drop 2 c) :: Int
   in case (head c, head (tail c)) of
        ('x', '<') -> ('x', (1, v - 1))
        ('m', '<') -> ('m', (1, v - 1))
        ('a', '<') -> ('a', (1, v - 1))
        ('s', '<') -> ('s', (1, v - 1))
        ('x', '>') -> ('x', (v + 1, 4000))
        ('m', '>') -> ('m', (v + 1, 4000))
        ('a', '>') -> ('a', (v + 1, 4000))
        ('s', '>') -> ('s', (v + 1, 4000))
        _ -> error $ "Invalid " ++ c

parse :: String -> ([Part], [Workflow])
parse s = do
  let [w, p] = splitOn "\n\n" s
  let worklows = map parseWorkflow (lines w)
  let parts = map parsePart (lines p)
  (parts, worklows)

parsePart :: String -> Part
parsePart s =
  let [x', m', a', s'] = splitOn "," (init (tail s))
   in Part (read $ drop 2 x') (read $ drop 2 m') (read $ drop 2 a') (read $ drop 2 s')

parseWorkflow :: String -> Workflow
parseWorkflow l = Workflow name (map parseCondition conds)
  where
    [name, rest] = splitOn "{" l
    conds = splitOn "," (init rest)

parseCondition :: String -> Condition
parseCondition c = case splitOn ":" c of
  [s] -> Lit s
  [v, s] -> Con (parseCondition' v) s

parseCondition' :: [Char] -> PartPredicate
parseCondition' c =
  let v = read (drop 2 c) :: Int
   in case (head c, head (tail c)) of
        ('x', '<') -> \p -> x p < v
        ('m', '<') -> \p -> m p < v
        ('a', '<') -> \p -> a p < v
        ('s', '<') -> \p -> s p < v
        ('x', '>') -> \p -> x p > v
        ('m', '>') -> \p -> m p > v
        ('a', '>') -> \p -> a p > v
        ('s', '>') -> \p -> s p > v
        _ -> error $ "Invalid " ++ c
