module Day7(run) where

import Data.List.Split
import Data.Maybe
import Data.Map (Map, empty, elems, (!), notMember, fromList, insert)

run = do lines <- readLines
         let reverse = buildReverseLookup (elems lines) empty
             top = until (\k -> notMember k reverse) (\k -> reverse ! k) (head $ elems reverse)
             tree = buildTree lines top
          in return (top, findInbalance tree)

readLines :: IO (Map String Line)
readLines = do lines <- map parseLine . splitOn "\n" <$> readFile "data/day7.txt"
               return $ fromList $ zip (map name lines) lines

-- A line has a name, a weight and a list of references
data Line = Line { name :: String,
                   weight :: Int,
                   references :: [String]
                 } deriving (Show, Eq, Ord)

parseLine :: String -> Line
parseLine chars
    = let (n, rest1) = span (/=' ') chars
          (w, rest2) = span (/=')') $ drop 2 rest1
          rs = filter (/="") $ splitOn ", " $ drop 5 rest2
       in Line{ name=n, weight=(read w), references=rs }

buildReverseLookup :: [Line] -> Map String String -> Map String String
buildReverseLookup (line:lines) map 
    = let nextMap = foldl (\m v -> insert v (name line) m) map $ references line
        in buildReverseLookup lines nextMap
buildReverseLookup [] map = map

-- For building the real tree
data Tree = Tree { root :: String, total :: Int, own :: Int, balanced :: Bool, branches :: [Tree] } deriving (Show, Eq)

buildTree :: Map String Line -> String -> Tree
buildTree lines key = let line = lines ! key
                          branches = map (buildTree lines) $ references line
                          totalWeight = weight line + (sum $ map total branches)
                          balanced = and $ map (\x -> (total x) == (total $ head branches)) branches
                       in Tree { root=key, total=totalWeight, own=(weight line), balanced=balanced, branches=branches }

findInbalance :: Tree -> Maybe (String, Int)
findInbalance tree@(Tree _ _ _ True branches) = Nothing
findInbalance tree@(Tree _ _ _ False branches) 
    = let inbalancedChilds = catMaybes $ map findInbalance branches
       in if inbalancedChilds /= []
          then Just $ head inbalancedChilds
          else findOddChild branches

findOddChild :: [Tree] -> Maybe (String, Int)
findOddChild trees = findOdd (Nothing, Nothing) trees

findOdd :: (Maybe Tree, Maybe Tree) -> [Tree] -> Maybe (String, Int)
findOdd (Nothing, Nothing) (tree:trees) = findOdd (Just tree, Nothing) trees
findOdd (Just a, Nothing) (tree:trees)
   | total a == total tree = findOdd (Just a, Nothing) trees
   | otherwise             = findOdd (Just a, Just tree) trees
findOdd (Just a, Just b) (tree:trees)
   | total a == total tree = Just (root b, (total a) - (total b) + (own b))
   | total b == total tree = Just (root a, (total b) - (total a) + (own a))
   | otherwise             = Nothing
findOdd _ _ = Nothing
