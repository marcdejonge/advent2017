module Day2(run) where

import Data.List.Split

run = do numbers <- readNumbers
         return (part1 numbers, part2 numbers)

readNumbers :: IO [[Int]]
readNumbers = map (map read . splitOn "\t") <$> splitOn "\n" <$> readFile "data/day2.txt"

part1 numbers = numbers
part2 _ = "Not yet implemented"
