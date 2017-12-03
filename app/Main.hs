module Main where

import System.IO
import qualified Day1

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
        1 -> Day1.run
        _ -> fail "Unknown day"

writeDayResults :: (Show a, Show b) => Int -> (a, b) -> String
writeDayResults day (part1, part2) = "Results of day " ++ (show day) ++ ", part1: " ++ (show part1) ++ " part2: " ++ (show part2)
