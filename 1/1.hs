import Data.List

main = do
    contents <- getContents
    print . sum . snd . foldl (topNElves 3) (0, []) . lines $ contents


topNElves :: Int -> (Int, [Int]) -> String -> (Int, [Int])
topNElves n (acc, maxes) "" = (0, take n . reverse . sort $ (acc : maxes))
topNElves n (acc, maxes) numStr = (acc + read numStr, maxes)