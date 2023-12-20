module Day11 (run) where

type Point = (Int, Int)

run :: IO ()
run = do
  ls <- lines <$> readFile "src/day11.txt"
  let (emptyRows, emptyCols) = (getEmptyRows ls, getEmptyCols 0 ls)
  let mp = filter (\(a, b) -> b == '#') $ parseLines 2 ls emptyRows emptyCols
  let galaxies = zip [1 ..] $ map fst mp
  let dists = [(g1, g2, manhattanDistance g1 g2) | (g1i, g1) <- galaxies, (g2i, g2) <- galaxies, g1i < g2i]
  print $ "Day 11, Part 1: " ++ show (sum $ map (\(_, _, c) -> c) dists)
  let mp' = filter (\(a, b) -> b == '#') $ parseLines 1000000 ls emptyRows emptyCols
  let galaxies' = zip [1 ..] $ map fst mp'
  let dists' = [(g1, g2, manhattanDistance g1 g2) | (g1i, g1) <- galaxies', (g2i, g2) <- galaxies', g1i < g2i]
  print $ "Day 11, Part 2: " ++ show (sum $ map (\(_, _, c) -> c) dists')
  print "Done"

manhattanDistance :: Point -> Point -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

parseLines :: Int -> [String] -> [Int] -> [Int] -> [(Point, Char)]
parseLines growth lines emptyRows emptyCols = parseLines' 0 0 lines
  where
    parseLines' y i [] = []
    parseLines' y i (row : rows)
      | i `elem` emptyRows = parseLines' (y + growth) (i + 1) rows
      | otherwise = parseLine y 0 0 row ++ parseLines' (y + 1) (i + 1) rows
    parseLine y x i [] = []
    parseLine y x i (v : vs)
      | i `elem` emptyCols = parseLine y (x + growth) (i + 1) vs
      | otherwise = ((x, y), v) : parseLine y (x + 1) (i + 1) vs

getEmptyRows :: [String] -> [Int]
getEmptyRows rows = map fst $ filter (\(y, v) -> all (== '.') v) $ zip [0 ..] rows

getEmptyCols :: Int -> [String] -> [Int]
getEmptyCols x cols
  | x >= length cols = []
  | all (== '.') [r !! x | r <- cols] = x : getEmptyCols (x + 1) cols
  | otherwise = getEmptyCols (x + 1) cols