module Day4(run) where

import Data.List
import Data.List.Split

run = do passwords <- readPasswords
         return (part1 passwords, part2 passwords)

readPasswords :: IO [[String]]
readPasswords = map (splitOn " ") <$> splitOn "\n" <$> readFile "data/day4.txt"

part1 passwords = sum $ map (fromEnum . checkPassword) passwords

part2 passwords = sum $ map (fromEnum . checkPassword) $ map (map sort) passwords

checkPassword :: [String] -> Bool
checkPassword (pass:passes) = (not (pass `elem` passes)) && checkPassword passes
checkPassword [] = True
