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


part2 numbers = sum $ map divisors numbers

divisors :: [Int] -> Int
divisors (x:xs) = divisor x xs + divisors xs
divisors [] = 0

divisor :: Int -> [Int] -> Int
divisor y (x:xs)
    | y < x && x `rem` y == 0 = x `div` y + divisor y xs
    | y > x && y `rem` x == 0 = y `div` x + divisor y xs
    | otherwise               = divisor y xs
divisor _ [] = 0
