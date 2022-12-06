import Data.Maybe

import Data.IntMap.Strict (IntMap, (!), insert)
import qualified Data.IntMap.Strict as IntMap

data Stack = Empty | Box Char Stack deriving Show
data Instruction = Instruction { quantity :: Int, from :: Int, to :: Int } deriving Show

type MoveF = Int -> (Stack, Stack) -> (Stack, Stack)

main = do
    contents <- getContents
    let allLines = lines contents
        (cratesExtra, instructionsExtra) = break (== "") allLines
        crates = IntMap.fromList . zip [1..] . parseCrates $ init cratesExtra
        instructions = map parseInstruction $ tail instructionsExtra

    print . IntMap.elems . IntMap.map (\(Box c _) -> c) . foldl (applyInstruction moveN') crates $ instructions



applyInstruction :: MoveF -> IntMap Stack -> Instruction -> IntMap Stack
applyInstruction _ s (Instruction 0 _ _) = s
applyInstruction mf s (Instruction qnt from to) = s'
    where fromStack = s ! from
          toStack = s ! to
          (fromStack', toStack') = mf qnt (fromStack, toStack)
          s' = insert to toStack' (insert from fromStack' s)

moveN :: MoveF
moveN 0 ss = ss
moveN n (from, to) = moveN (n-1) (from', to')
    where (Box c from') = from
          to' = Box c to

moveN' :: MoveF
moveN' 0 ss = ss
moveN' n (Box c from', to) = (from'', to'')
    where (from'', to') = moveN' (n-1) (from', to)
          to'' = Box c to'



parseCrates :: [String] -> [Stack]
parseCrates lines = map (readStack crates) [0..(length $ head crates)-1]
    where crates = map parseLine lines

readStack :: [[Maybe Char]] -> Int -> Stack
readStack [] _ = Empty
readStack (l:ls) n
    | slot == Nothing = rest
    | otherwise       = Box (fromJust slot) rest
    where slot = l !! n
          rest = readStack ls n

parseLine :: String -> [Maybe Char]
parseLine [] = []
parseLine (_:c:_:xs) = crate:parseLine (drop 1 xs)
    where crate = if c == ' ' then Nothing else Just c

parseInstruction :: String -> Instruction
parseInstruction line = Instruction quantity from to
    where ws = words line
          quantity = read (ws !! 1)
          from = read (ws !! 3)
          to = read (ws !! 5)