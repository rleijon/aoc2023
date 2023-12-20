module Day9 (run) where

run :: IO ()
run = do
  ls <- map parseSequence . lines <$> readFile "src/day9.txt"
  let vs = map findNextValue ls
  print $ "Day 9, Part 1: " ++ show (sum (map last vs))
  let vs' = map findFirstValue ls
  print $ "Day 9, Part 2: " ++ show (sum (map head vs'))
  print "Done"

findFirstValue :: [Int] -> [Int]
findFirstValue line = do
  let allDerivatives = map head $ findAllDerivatives [line]
  let firstValue = foldl (flip (-)) 0 allDerivatives
  firstValue : line

findNextValue :: [Int] -> [Int]
findNextValue line = do
  let allDerivatives = findAllDerivatives [line]
  line ++ [sum [last x | x <- allDerivatives, not (null x)]]

findAllDerivatives :: [[Int]] -> [[Int]]
findAllDerivatives x = let a = findDerivative (head x) in if all (== 0) a then x else findAllDerivatives (a : x)

findDerivative :: [Int] -> [Int]
findDerivative ls = zipWith (-) (tail ls) ls

parseSequence :: String -> [Int]
parseSequence line = (read :: String -> Int) <$> words line
