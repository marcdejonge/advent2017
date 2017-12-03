module Day1(run) where

import Data.Char

run = do numbers <- readNumbers
         return (part1 numbers, part2 numbers)

readNumbers :: IO [Int]
readNumbers = map digitToInt <$> readFile "data/day1.txt"

part1 :: [Int] -> Int
part1 numbers = sum $ map saveSame $ zip numbers $ tail numbers ++ [head numbers]

part2 :: [Int] -> Int
part2 numbers = 2 * (sum $ map saveSame $ uncurry zip $ splitAt ((length numbers + 1) `div` 2) numbers)

saveSame (x, y) = if x == y then x else 0
