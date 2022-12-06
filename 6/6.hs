import Data.List

main = do
    contents <- getContents
    print $ markerIndex contents

markerIndex :: String -> Int
markerIndex = markerIndex' ""

markerIndex' :: String -> String -> Int
markerIndex' last (c:cs)
    | isMarker  = 1
    | otherwise = 1 + (markerIndex' candidate cs)
    where
        candidate = c : (take 13 last)
        isMarker = (length $ nub candidate) == 14