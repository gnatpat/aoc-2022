import Data.List
import Data.Char

main = mainB

mainA = do
  contents <- getContents
  print . sum . map (priority . head . uncurry intersect . splitLine) $ lines contents

mainB = do
  contents <- getContents
  print . sum . map (priority .  head . intersectAll) . chunksOf 3 $ lines contents


splitLine :: String -> (String, String)
splitLine l = splitAt (div (length l) 2) l


priority :: Char -> Int
priority c
  | 'a' <= c && c <= 'z' = (ord c) - (ord 'a') + 1
  | otherwise            = (ord c) - (ord 'A') + 27


chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = (take n l):(chunksOf n (drop n l))
  

intersectAll :: Eq a => [[a]] -> [a]
intersectAll [] = []
intersectAll [x] = x
intersectAll (x:xs) = intersect x (intersectAll xs)
