import Data.List

import Data.Map.Strict(Map)
import qualified Data.Map.Strict as Map

data File = File String Int deriving Show
data Dir = Dir [File] (Map String Dir) deriving Show
type State = (Dir, [String])

data LsResult = FileEntry String Int | DirEntry String deriving Show
data Command = Cd String | Ls [LsResult] deriving Show

main = do
    contents <- getContents
    let dir = fst . foldl perform ((Dir [] Map.empty), []) . parseInput . lines $ contents
        topLevel = fst $ sizeSearch dir
        unused = 70000000 - topLevel
        toDelete = 30000000 - unused
    print . snd $ sizeSearch dir
    print $ sizeSearch' toDelete dir


parseInput :: [String] -> [Command]
parseInput [] = []
parseInput (c:ls) 
    | command == "cd" = (Cd (head rest)) : (parseInput ls)
    | otherwise       = (Ls lsResults) : (parseInput ls')
    where
        (_:command:rest) = words c
        (resultsStrs, ls') = break ((== '$') . head) ls
        lsResults = map parseLsResult resultsStrs


parseLsResult :: String -> LsResult
parseLsResult s
    | s1 == "dir" = DirEntry s2
    | otherwise    = FileEntry s2 (read s1)
    where
        [s1, s2] = words s

perform :: State -> Command -> State
perform (topLevel, path) (Cd "/") = (topLevel, [])
perform (topLevel, path) (Cd "..") = (topLevel, init path)
perform (topLevel, path) (Cd folder) = (topLevel, path ++ [folder])
perform (topLevel, path) (Ls results) = (addResults path results topLevel, path)

addResults :: [String] -> [LsResult] -> Dir -> Dir
addResults [] r d = addResults' r d
addResults (p:ps) r (Dir fs folders) = Dir fs (Map.adjust (addResults ps r) p folders)


addResults' :: [LsResult] -> Dir -> Dir
addResults' [] d = d
addResults' ((FileEntry name size):rs) (Dir fs dirs) = addResults' rs d'
    where d' = Dir ((File name size):fs) dirs
addResults' ((DirEntry name):rs) (Dir fs dirs) = addResults' rs d'
    where d' = Dir fs (Map.insert name (Dir [] Map.empty) dirs)

sizeSearch :: Dir -> (Int, Int)
sizeSearch (Dir files folders) = (size, allResults)
    where
        fileSizes = sum $ map fileSize files
        children = map sizeSearch $ Map.elems folders
        childResults = sum $ map snd children
        childSizes = sum $ map fst children
        size = fileSizes + childSizes
        allResults = if size < 100000 then (size + childResults) else childResults

sizeSearch' :: Int -> Dir -> Int
sizeSearch' size (Dir files folders) =
    if (not $ null bigEnoughChildren) then
        head $ sort bigEnoughChildren
    else
        folderSize
    where
        fileSizes = sum $ map fileSize files
        children = map (sizeSearch' size) $ Map.elems folders
        bigEnoughChildren = filter (> size) children
        childSizes = sum children
        folderSize = fileSizes + childSizes

fileSize :: File -> Int
fileSize (File _ size) = size