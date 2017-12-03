module Main where

import qualified Day1

main :: IO ()
main = do day1 <- Day1.run
          putStrLn (writeDayResults 1 day1)

writeDayResults :: (Show a, Show b) => Int -> (a, b) -> String
writeDayResults day (part1, part2) = "Results of day " ++ (show day) ++ ", part1: " ++ (show part1) ++ " part2: " ++ (show part2)
