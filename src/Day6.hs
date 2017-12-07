module Day6(run) where

import Data.List.Split
import Data.Array

run = do numbers <- readNumbers
         return (part1 numbers, part2 numbers)

readNumbers :: IO (Array Int Int)
readNumbers = do numbers <- (map read . splitOn "\t") <$> readFile "data/day6.txt"
                 return $ listArray (1, length numbers) numbers

part1 numbers = numbers
part2 _ = "Not yet implemented"


