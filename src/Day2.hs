module Day2 (run) where
import Data.Char (isDigit, digitToInt)
import Data.List (findIndex, isPrefixOf, isSuffixOf)
import Control.Concurrent (waitQSem)
import Data.Maybe (fromJust, isJust)
import Debug.Trace (trace)
import qualified Data.Map as Map
import Data.List.Split (splitOn)
import Text.Parsec (label)

type Turn = (Int, Int, Int)
data Game = Game Int [Turn] deriving Show

run :: IO ()
run = do
    games <- map parseLine . lines <$> readFile "src/day2.txt"
    let possibleGames = filter (isPossible (12,13,14)) games
    let maximumGames = sum (map maximumRequired games)
    print $ "Day 2, Part 1: " ++ show (sum (map (\(Game i _) -> i) possibleGames))
    print $ "Day 2, Part 2: " ++ show maximumGames
    print "Done"

maximumRequired :: Game -> Int
maximumRequired (Game _ tns) = let (r,g,b) = maximumRequired' (0,0,0) tns in r * g * b
    where
        maximumRequired' (r,g,b) [] = (r,g,b)
        maximumRequired' (r,g,b) ((ra,ga,ba):xs) = maximumRequired' (max r ra,max g ga,max b ba) xs

isPossible :: Turn -> Game -> Bool
isPossible (r,g,b) (Game _ tns) = and [ra <= r && ga <= g && ba <= b|(ra,ga,ba)<-tns]

parseLine :: String -> Game
parseLine line = Game g (map parseTurn tns)
    where
        [p, s] = splitOn ": " line
        g = read (drop 5 p) :: Int
        tns = splitOn "; " s
    

parseTurn :: [Char] -> Turn
parseTurn t = (Map.findWithDefault 0 "red" ts, Map.findWithDefault 0 "green" ts, Map.findWithDefault 0 "blue" ts)
    where
        a = splitOn ", " t
        ts =  Map.fromList $ map (\v -> let [c, clr] = words v in (clr, read c :: Int)) a
