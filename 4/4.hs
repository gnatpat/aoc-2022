type Interval = (Int, Int)

main = do
    contents <- getContents
    print . length . filter overlap . map parseLine . lines $ contents

splitOn :: Char -> String -> [String]
splitOn c [] = []
splitOn c [x] 
    | c == x    = [[]]
    | otherwise = [[x]]
splitOn c (x:xs)
    | c == x    = [] : splitOn c xs
    | otherwise = (x : w) : ws
                where w:ws = splitOn c xs

parseLine :: String -> (Interval, Interval)
parseLine l = (parseInterval a, parseInterval b)
              where [a, b] = splitOn ',' l

parseInterval :: String -> Interval
parseInterval i = (read a, read b)
                  where [a, b] = splitOn '-' i

fullyContains :: (Interval, Interval) -> Bool
fullyContains ((s1, e1), (s2, e2)) = (s1 <= s2 && e1 >= e2) || (s2 <= s1 && e2 >= e1)

overlap :: (Interval, Interval) -> Bool
overlap ((s1, e1), (s2, e2)) = (s1 <= e2 && e1 >= s2) || (s2 <= e1 && e2 >= s1)