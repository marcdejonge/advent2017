module Day6(run) where

import Data.List.Split
import qualified Data.Map as Map

run = do numbers <- readNumbers
         let (first, second) = findDuplicate $ calcNexts numbers
          in return (second, second - first)

readNumbers :: IO [Int]
readNumbers = (map read . splitOn "\t") <$> readFile "data/day6.txt"

calcNexts :: [Int] -> [[Int]]
calcNexts xs = iterate (calcNext $ length xs) xs

calcNext :: Int -> [Int] -> [Int]
calcNext length xs
    =  let (maxIx, max)   = findMaxIx xs -- Determine the maximum number and index on which it happens
           (adder, extra) = max `quotRem` length -- How many to add to each, plus the number of extra
           until          = (maxIx + extra) `rem` length -- Until which index should we add one extra
        in add xs 1 (maxIx, until, adder)

findMaxIx :: [Int] -> (Int, Int)
findMaxIx xs = foldl1 maxOfPair $ zip [1..] xs
maxOfPair x@(_,a) y@(_,b) = if b > a then y else x

add :: [Int] -> Int -> (Int, Int, Int) -> [Int]
add (x:xs) ix opts@(maxIx, untilIx, adder)
    = let next = if ix == maxIx then adder
                 else if untilIx <  maxIx && (ix <= untilIx || ix > maxIx) then x + adder + 1
                 else if untilIx >= maxIx && (ix <= untilIx && ix > maxIx) then x + adder + 1
                 else x + adder
       in next:(add xs (ix + 1) opts)
add [] _ _ = []

findDuplicate xs = findDuplicate' Map.empty $ zip [0..] xs
findDuplicate' found ((ix,value):xs)
    = let prev = Map.lookup value found
          nextFound = Map.insert value ix found
       in maybe (findDuplicate' nextFound xs) (\prevIx -> (prevIx, ix)) prev
