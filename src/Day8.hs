module Day8(run) where
    
import Prelude hiding (lookup)
import Data.List.Split (splitOn)
import Data.Map (Map,insert,lookup,empty)
import Data.Maybe (fromMaybe)

run = do instructions <- readInstructions
         let states = scanl runInstruction empty instructions
         return (getMax $ last states, getMax $ map getMax $ states)

getMax state = foldl max 0 state

readInstructions :: IO [Instruction]
readInstructions = map parseInstruction <$> splitOn "\n" <$> readFile "data/day8.txt"

data Instruction = Instruction{ var :: String, 
                                func :: (Int -> Int), 
                                checkVar :: String, 
                                test :: (Int -> Bool)
                              }

parseInstruction :: String -> Instruction
parseInstruction input
    = let (v:id:m:f:c:t:tv:_) = splitOn " " input
          func = case id of
                    "inc" -> (+ (read m))
                    "dec" -> (subtract (read m))
                    _      -> fail "Unknown function type"
          testVar = read tv :: Int
          test = case t of
                    ">"  -> (> testVar)
                    "<"  -> (< testVar)
                    ">=" -> (>= testVar)
                    "<=" -> (<= testVar)
                    "==" -> (== testVar)
                    "!=" -> (/= testVar)
                    _    -> fail "Unknown comparison type"
       in Instruction{ var=v, func=func, checkVar=c, test=test }

valueOf :: Map String Int -> String -> Int
valueOf store key = fromMaybe 0 $ lookup key store

runInstruction :: Map String Int -> Instruction -> Map String Int
runInstruction store instr
            = let testValue = valueOf store $ checkVar instr
                  updateValue = valueOf store $ var instr
               in if test instr $ testValue 
                  then insert (var instr) (func instr $ updateValue) store  
                  else store
