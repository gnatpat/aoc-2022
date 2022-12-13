import Data.Char
import Data.Maybe
import Data.List
import Debug.Trace

import Data.Map.Strict (Map, (!), (!?))
import qualified Data.Map.Strict as Map

import Data.Set (Set)
import qualified Data.Set as Set

type Coord = (Int, Int)

-- d s x = trace (s ++ ": " ++ show x) x
d s x = x

mainA = do
  contents <- getContents
  let hs = parseInput contents
      start = head $ findChar 'S' contents
      goal = head $ findChar 'E' contents
      unvisited = Set.delete start $ Map.keysSet hs
      ds = Map.singleton start 0
      ds' = pathfind hs unvisited ds start
  print goal
  print $ Map.lookup goal ds'

main = do
  contents <- getContents
  let hs = parseInput contents
      start = head $ findChar 'E' contents
      unvisited = Set.delete start $ Map.keysSet hs
      ds = Map.singleton start 0
      ds' = pathfind hs unvisited ds start
      low = Set.fromList $ findChar 'a' contents
      lowDist = sortOn snd . filter ((flip Set.member) low . fst) $ Map.toList ds' 
  print low
  print lowDist

height :: Char -> Int
height 'S' = 0
height 'E' = 25
height c = ord(c) - ord('a')

toCoord' :: Int -> [(Int, Char)] -> [((Int, Int), Char)]
toCoord' y l = map (\(x, c) -> ((x, y), c)) l

toCoord :: Int -> [(Int, Char)] -> [((Int, Int), Int)]
toCoord y l = map (\(x, c) -> ((x, y), height c)) l

parseInput :: String -> Map Coord Int
parseInput = Map.fromList . concat . zipWith toCoord  [0..] . map (zip [0..]) . lines

findChar :: Char -> String -> [Coord]
findChar c = map fst . filter ((==c) . snd) . concat . zipWith toCoord'  [0..] . map (zip [0..]) . lines

pathfind :: Map Coord Int -> Set Coord -> Map Coord Int -> Coord -> Map Coord Int
pathfind hs unvisited ds current = if null unvisited' || null candidates then ds' else next
  where ns = filter (flip Set.member unvisited) . connected hs $ (d "current" current)
        d' = 1 + (ds Map.! current)
        ds' = foldl (insertMin d') ds (d "ns" ns)
        unvisited' = Set.delete current unvisited
        candidates = sortOn snd . filter ((`Set.member` (d "unvisisted'" unvisited')) . fst) . Map.toList $ (d "ds'" ds')
        close = fst . head $ candidates
        next = pathfind hs unvisited' ds' close
        
insertMin :: Int -> Map Coord Int -> Coord -> Map Coord Int
insertMin d ds c = Map.insertWith min c d ds

connected :: Map Coord Int -> Coord -> [Coord]
connected m c = filter (isConnected m c) $ neighbours c

neighbours :: Coord -> [Coord]
neighbours (x, y) = [(x+x', y+y') | x'<-[-1..1], y'<-[-1..1], abs(x') + abs(y') == 1]

isConnected :: Map Coord Int -> Coord -> Coord -> Bool
isConnected m src dest = case maybeClose of Just x -> x
                                            Nothing -> False
  where maybeClose = close <$> (m !? src) <*> (m !? dest) 

close :: Int -> Int -> Bool
close src dest = (dest - src) >= -1
