module Day5(run) where

import Data.List.Split
import Control.Monad.ST
import Data.Array.IO
import Data.Array.ST

run = do numbers <- readNumbers
         return (part1 numbers, part2 numbers)

readNumbers :: IO [Int]
readNumbers = (map read . splitOn "\n") <$> readFile "data/day5.txt"

part1 = calcSteps (+1)

part2 = calcSteps (\x -> if x >= 3 then x - 1 else x + 1)

calcSteps :: (Int -> Int) -> [Int] -> Int
calcSteps update numbers = runST $ do 
           array <- newListArray (1, length numbers) numbers :: ST s (STUArray s Int Int)
           step update 1 array (length numbers)

step update ix array length
  | ix > length = return 0
  | ix <= 0     = fail "ix <= 0"
  | otherwise   = do curr <- readArray array ix
                     writeArray array ix (update curr)
                     next <- step update (ix + curr) array length
                     return (next + 1)
