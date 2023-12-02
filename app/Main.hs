module Main where

import System.Environment (getArgs)
import qualified Day1
import qualified Day2

main :: IO ()
main = do
    args <- getArgs
    let day = read (head args)
    case day of
        1 -> Day1.run
        2 -> Day2.run
        _ -> error $ "Unknown day: " ++ show day