module Day3(run,findDistanceFromCenter) where

import Data.Maybe

run = let number = 277678
       in (part1 number, part2 number)

part1 :: Int -> Int
part1 number = let ring  = ceiling (((sqrt $ fromIntegral number) - 1) / 2)
                   end   = (2 * ring + 1) ^ 2
                   step  = ring * 2 -- The length of de side of the square
                in findDistanceFromCenter end step number + ring

findDistanceFromCenter :: Int -> Int -> Int -> Int
findDistanceFromCenter end step number
    | (end - step) < number = abs ((end - (step `div` 2)) - number)
    | otherwise             = findDistanceFromCenter (end - step) step number

part2 number = findNumberGen number [1,2,4,5,10,11,23,25]

-- Sholud be 279138, easy to find @ https://oeis.org/A141481/b141481.txt

findNumberGen number xs = let test = findNumber number xs
                           in if isJust test 
                              then fromJust test 
                              else findNumberGen number $ generateCircle xs

findNumber number (x:xs) = if x > number then Just x else findNumber number xs
findNumber number [] = Nothing

generateCircle xs = generateCircle' ((length xs `div` 4) + 2) 0 0 0 (last xs) xs
generateCircle' sideLength ix side first last (x:xs)
    | side > 3             = []
    | ix == 0 && side == 0 = let next = last + x
                              in next: generateCircle' sideLength (ix + 1) side next next (last:x:xs)
    | ix == 0 && side > 0  = let next = 2 * last + head(xs)
                              in next: generateCircle' sideLength (ix + 1) side first next (x:xs)
    | ix == sideLength - 1 = let next = last + x + (if side == 3 then first else 0)
                              in next: generateCircle' sideLength 0 (side + 1) first next (x:xs)
    | ix == sideLength - 2 = let next = last + x + head(xs) + (if side == 3 then first else 0)
                              in next: generateCircle' sideLength (ix + 1) side first next xs
    | otherwise            = let next = last + x + head(xs) + head(tail(xs))
                              in next: generateCircle' sideLength (ix + 1) side first next xs
