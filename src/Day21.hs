module Day21 where

run :: IO ()
run = do
  ls' <- lines <$> readFile "src/day21.txt"
  print $ "Day 21, Part 1: " ++ show 1
  print $ "Day 21, Part 2: " ++ show 2
  print "Done"