module Main where

import System.IO
import qualified Day1
import qualified Day2

prompt msg = do putStr msg
                hFlush stdout
                getLine

main :: IO ()
main = do day <- prompt "Which day do you want to run? "
          result <- runDay $ read day
          putStrLn result

runDay :: Int -> IO String
runDay day = writeDayResults 1 <$> mapNr day

mapNr day =
    case day of
        1 -> showPair <$> Day1.run
        2 -> showPair <$> Day2.run
        _ -> fail "Unknown day"

showPair :: (Show a, Show b) => (a, b) -> (String, String)
showPair (a, b) = (show a, show b)

writeDayResults :: Int -> (String, String) -> String
writeDayResults day (part1, part2) = "Results of day " ++ (show day) ++ ", part1: " ++ part1 ++ " part2: " ++ part2
