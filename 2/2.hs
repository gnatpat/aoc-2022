data Shape = Rock | Paper | Scissors
data Outcome = Lose | Draw | Win

main = mainB

mainA = do
    contents <- getContents
    print . sum . map (points . parseLine) . lines $ contents

mainB = do
    contents <- getContents
    print . sum . map (points2 . parseLine2) . lines $ contents

parseLine :: String -> (Shape, Shape)
parseLine line = (parseShape $ line !! 0, parseShape $ line !! 2)

parseLine2 :: String -> (Shape, Outcome)
parseLine2 line = (parseShape $ line !! 0, parseOutcome $ line !! 2)

parseShape :: Char -> Shape
parseShape 'A' = Rock
parseShape 'B' = Paper
parseShape 'C' = Scissors
parseShape 'X' = Rock
parseShape 'Y' = Paper
parseShape 'Z' = Scissors

parseOutcome :: Char -> Outcome
parseOutcome 'X' = Lose
parseOutcome 'Y' = Draw
parseOutcome 'Z' = Win

shapePoints:: Shape -> Int
shapePoints Rock = 1
shapePoints Paper = 2
shapePoints Scissors = 3

outcome :: (Shape, Shape) -> Outcome
outcome (Rock, Rock) = Draw
outcome (Paper, Paper) = Draw
outcome (Scissors, Scissors) = Draw
outcome (Rock, Paper) = Lose
outcome (Rock, Scissors) = Win
outcome (Paper, Rock) = Win
outcome (Paper, Scissors) = Lose
outcome (Scissors, Rock) = Lose
outcome (Scissors, Paper) = Win

outcomePoints :: Outcome -> Int
outcomePoints Lose = 0
outcomePoints Draw = 3
outcomePoints Win = 6

points :: (Shape, Shape) -> Int
points (op, you) = (outcomePoints $ outcome (you, op)) + shapePoints you

yourChoice :: (Shape, Outcome) -> Shape
yourChoice (Rock, Draw) = Rock
yourChoice (Paper, Draw) = Paper
yourChoice (Scissors, Draw) = Scissors
yourChoice (Rock, Win) = Paper
yourChoice (Rock, Lose) = Scissors
yourChoice (Paper, Lose) = Rock
yourChoice (Paper, Win) = Scissors
yourChoice (Scissors, Win) = Rock
yourChoice (Scissors, Lose) = Paper

points2 :: (Shape, Outcome) -> Int
points2 (s, o) = outcomePoints o + (shapePoints $ yourChoice (s, o))
