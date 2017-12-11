module Day9(run) where
    
run = do input <- readFile "data/day9.txt"
         return $ parse 0 input

parse :: Int -> String -> (Int, Int)
parse open ('{':rest)   = parse (open + 1) rest
parse open ('!':_:rest) = parse open rest
parse open ('<':rest)   = let (charCount, restNew) = readComment 0 rest
                              (opened, charCountOld) = parse open restNew
                           in (opened, charCount + charCountOld)
parse 0    ('}':rest)   = parse 0 rest
parse open ('}':rest)   = let (openOld, charCount) = parse (open - 1) rest
                           in (openOld + open, charCount)
parse open (',':rest)   = parse open rest
parse _    []           = (0, 0)

readComment :: Int -> String -> (Int, String)
readComment charCount ('>':rest)   = (charCount, rest)
readComment charCount ('!':_:rest) = readComment charCount rest
readComment charCount (_:rest)     = readComment (charCount + 1) rest
