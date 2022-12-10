import Debug.Trace

data Instruction = Noop | AddX Int deriving Show

main = do
  contents <- getContents
  print . getStateAt [20,60..220] (1, 1) . map parseLine $ lines contents

parseLine :: String -> Instruction
parseLine s = case words s of ["noop"] -> Noop
                              ["addx", x] -> AddX (read x)

getStateAt :: [Int] -> (Int, Int) -> [Instruction] -> [Int]
getStateAt [] (value, time) _ = []
getStateAt _ (value, time) [] = error "run out of instructions"
getStateAt (a:as) (value, time) (i:is)
  | (trace (show time) time) >= a = ((trace (show a) a) * (trace (show value) value)) : (getStateAt as (value', time') is)
  | otherwise = getStateAt (a:as) (value', time') is
  where
    (time', value') = case (trace (show i) i) of Noop -> (time + 1, value)
                                                 AddX i -> (time + 2, value + i)
                      
  
