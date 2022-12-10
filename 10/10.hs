import Data.List

data Instruction = Noop | AddX Int deriving Show

main = do
  contents <- getContents
  print . getStateAt [20,60..220] (1, 1) . map parseLine $ lines contents
  sequence . map putStrLn . chunksOf 40 . reverse . getScr . foldl draw (State 1 "" 0) . map parseLine $ lines contents

parseLine :: String -> Instruction
parseLine s = case words s of ["noop"] -> Noop
                              ["addx", x] -> AddX (read x)

getStateAt :: [Int] -> (Int, Int) -> [Instruction] -> Int
getStateAt [] (value, time) _ = 0
getStateAt _ (value, time) [] = error "run out of instructions"
getStateAt (a:as) (value, time) (i:is)
  | time' > a = (a * value) + (getStateAt as (value', time') is)
  | otherwise = getStateAt (a:as) (value', time') is
  where
    (time', value') = case i of Noop -> (time + 1, value)
                                AddX i -> (time + 2, value + i)
                      
data State = State Int String Int deriving Show

getScr :: State -> String
getScr (State _ scr _) = scr

draw :: State -> Instruction -> State
draw (State x scr row) Noop = State x ((scrChar x row):scr) (nextRow row)
draw (State x scr row) (AddX a) = State (x+a) (c':c:scr) (nextRow row')
  where c = (scrChar x row)
        row' = nextRow row
        c' = (scrChar x row')


nextRow :: Int -> Int
nextRow row = (row + 1) `mod` 40

scrChar :: Int -> Int -> Char
scrChar x row = if x-1 <= row && row <= x+1 then '#' else ' '

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l = (take n l):(chunksOf n (drop n l))