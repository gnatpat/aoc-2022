import Data.List
import Data.Maybe

import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map

type Coord = (Int, Int)

main = do
  contents <- getContents
  let
    lookup = foldl parseRow Map.empty . zip [0..] $ lines contents
    width = length . head . lines $ contents
    height = length . lines $ contents
  print lookup
  print width
  print height
  let paths = concat [ups width height, downs width height, lefts width height, rights width height]
  
  print paths
  print . length . nub . concat $ map (seeCount lookup) paths
  print . maximum . map (score lookup) $ Map.toList lookup

parseRow :: Map Coord Int -> (Int, String) -> Map Coord Int
parseRow m (rowIndex, row) = foldl (addItem rowIndex) m $ (zip [0..]) row

addItem :: Int -> Map Coord Int -> (Int, Char) -> Map Coord Int
addItem rowIndex m (columnIndex, c) = Map.insert (rowIndex, columnIndex) (read [c]) m

downs width height = map (\x -> zip [0..height-1] $ repeat x) [0..width-1]
ups width height = map (\x -> zip [height-1,height-2..0] $ repeat x) [0..width-1]
lefts width height = map (\x -> zip (repeat x) [0..width-1]) [0..height-1]
rights width height = map (\x -> zip (repeat x) [width-1,width-2..0]) [0..height-1]

seeCount :: Map Coord Int -> [Coord] -> [Coord]
seeCount = seeCount' (-1)

seeCount' :: Int -> Map Coord Int -> [Coord] -> [Coord]
seeCount' _ _ [] = []
seeCount' tallest m (c:cs)
  | x > tallest = c : seeCount' x m cs
  | otherwise   = seeCount' tallest m cs
  where 
    x = m Map.! c

type Dir = Coord -> Coord
up :: Dir
up (y, x) = (y-1, x)

down :: Dir
down (y, x) = (y+1, x)

left :: Dir
left (y, x) = (y, x-1)

right :: Dir
right (y, x) = (y, x+1)

canSee :: Int -> Map Coord Int -> Coord -> Dir -> Int
canSee base m c dir 
  | isNothing maybeHeight = 0
  | height < base         = 1 + (canSee base m c' dir)
  | otherwise             = 1
  where
    c' = dir c
    maybeHeight = Map.lookup c' m
    height = fromJust maybeHeight

dirs = [up, down, left, right]
score :: Map Coord Int -> (Coord, Int) -> Int
score m (c, height) = product $ map (canSee height m c) dirs
