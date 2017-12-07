module Main where

import System.IO
import Data.Time.Clock
import Text.Printf
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6

prompt msg = do putStr msg
                hFlush stdout
                getLine

main :: IO ()
main = do day <- prompt "Which day do you want to run? "
          start <- getCurrentTime
          result <- runDay $ read day
          putStrLn result
          end <- getCurrentTime
          let diff = diffUTCTime end start
          printf "Computation time: %s\n" (show diff)

runDay :: Int -> IO String
runDay day = writeDayResults day <$> mapNr day

mapNr day =
    case day of
        1 -> showPair <$> Day1.run
        2 -> showPair <$> Day2.run
        3 -> return $ showPair Day3.run
        4 -> showPair <$> Day4.run
        5 -> showPair <$> Day5.run
        6 -> showPair <$> Day6.run
        _ -> fail "Unknown day"

showPair :: (Show a, Show b) => (a, b) -> (String, String)
showPair (a, b) = (show a, show b)

writeDayResults :: Int -> (String, String) -> String
writeDayResults day (part1, part2) = "Results of day " ++ (show day) ++ ", part1: " ++ part1 ++ " part2: " ++ part2
