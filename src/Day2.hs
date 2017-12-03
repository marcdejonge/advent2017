module Day2(run) where

import Data.List.Split

run = do numbers <- readNumbers
         return (part1 numbers, part2 numbers)

readNumbers :: IO [[Int]]
readNumbers = map (map read . splitOn "\t") <$> splitOn "\n" <$> readFile "data/day2.txt"

part1 numbers = sum $ map (maxDiff 0 0) numbers

maxDiff :: Int -> Int -> [Int] -> Int
maxDiff min max (x:xs)
    | min == 0 && max == 0 = maxDiff x x xs
    | x < min   = maxDiff x max xs
    | x > max   = maxDiff min x xs
    | otherwise = maxDiff min max xs
maxDiff min max [] = max - min


part2 _ = "Not yet implemented"
