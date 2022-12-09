import Data.List
import Data.Maybe

import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map

type Coord = (Int, Int)

main = do
  contents <- getContents
  print . snd . foldl applyMove (replicate 10 (0, 0), []) . concat . map parseMove $ lines contents
  print . length . nub . snd . foldl applyMove (replicate 10 (0, 0), []) . concat . map parseMove $ lines contents

add :: Coord -> Coord -> Coord
add (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

sub :: Coord -> Coord -> Coord
sub (a1, b1) (a2, b2) = (a1 - a2, b1 - b2)

up = (0, -1)
down = (0, 1)
left = (-1, 0)
right = (1, 0)

parseMove :: String -> [Coord]
parseMove s = replicate (read n) (parseDir dir)
  where [dir, n] = words s

parseDir :: String -> Coord
parseDir "U" = up
parseDir "D" = down
parseDir "L" = left
parseDir "R" = right


tailMove :: Coord -> Coord
tailMove (2, 0) = (1, 0)
tailMove (-2, 0) = (-1, 0)
tailMove (0, 2) = (0, 1)
tailMove (0, -2) = (0, -1)
tailMove (2, 1) = (1, 1)
tailMove (2, -1) = (1, -1)
tailMove (-2, 1) = (-1, 1)
tailMove (-2, -1) = (-1, -1)
tailMove (1, 2) = (1, 1)
tailMove (-1, 2) = (-1, 1)
tailMove (1, -2) = (1, -1)
tailMove (-1, -2) = (-1, -1)
tailMove (2, 2) = (1, 1)
tailMove (-2, -2) = (-1, -1)
tailMove (-2, 2) = (-1, 1)
tailMove (2, -2) = (1, -1)
tailMove _ = (0, 0)

applyMove :: ([Coord], [Coord]) -> Coord -> ([Coord], [Coord])
applyMove (knots, hist) dir = (knots', hist')
  where
    head' = (head knots) `add` dir
    knots' = moveKnots (tail knots) [head']
    hist' = (last knots') : hist

moveKnots :: [Coord] -> [Coord] -> [Coord]
moveKnots [] ks = reverse ks
moveKnots (k:ks) (l:ls) = moveKnots ks ((k `add` tailMove (l `sub` k)):l:ls)
