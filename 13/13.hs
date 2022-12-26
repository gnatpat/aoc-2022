import Control.Applicative (Alternative (..))
import Control.Monad (liftM, ap)
import Data.List (nub, sort, elemIndex)
import Data.Either (fromRight)

data Item = Value Int | List [Item] deriving (Show, Eq)

instance Ord Item where
  (<=) a b = toBool $ comp a b

mainA = do
  contents <- getContents
  print . sum . map fst . filter snd . zip [1..] . map (toBool . uncurry comp) . pairs $ lines contents

main = do
  contents <- getContents
  let
    packets = map parse . filter (/= "") . lines $ contents
    div1 = List [List [Value 2]]
    div2 = List [List [Value 6]]
    packets' = sort $ div1:div2:packets
  print $ (*) <$> ((+1) <$> elemIndex div1 packets') <*> ((+1) <$> elemIndex div2 packets')

parse :: String -> Item
parse = fst . fromRight undefined . runParser parseList

pairs [] = []
pairs (a:b:xs) = (parse a, parse b):(pairs $ drop 1 xs)

newtype Parser a = Parser
  { runParser :: String ->  Either [String] (a, String) }

instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure a = Parser $ \input -> Right (a, input)
  (<*>) = ap

instance Monad Parser where
  return = pure
  Parser p >>= k = Parser $ \input -> do
    (output, rest) <- p input
    runParser (k output) rest
      
instance Alternative Parser where
  empty = Parser $ \_ -> Left [""]

  Parser l <|> Parser r = Parser $ \input ->
    case l input of
      Left err ->
        case r input of
          Left err' -> Left $ nub $ err <> err'
          Right (output, rest) -> Right (output, rest)
      Right (output, rest) -> Right (output, rest)

satisfy :: (Char -> Bool) -> Parser Char
satisfy predicate = Parser $ \input ->
  case input of
    [] -> Left ["End of Input"]
    hd : rest
      | predicate hd -> Right (hd, rest)
      | otherwise    -> Left ["Unxpected " ++ [hd]]

char :: Char -> Parser Char
char i = satisfy (== i)

string :: String -> Parser String
string = traverse char

eof :: Parser ()
eof = Parser $ \input ->
  case input of
    [] -> Right ((), [])
    (x:_)  -> Left ["Expected end of file, got " ++ [x]]

numberChar :: Parser Char
numberChar = foldl1 (<|>) . map char $ ['0'..'9']

integer :: Parser Int
integer = read <$> some numberChar

between :: Parser a -> Parser b -> Parser c -> Parser c
between l r p = l *> p <* r

parseList :: Parser Item
parseList = List <$> between (char '[') (char ']')
  (((:) <$> parseItem <*> (many $ (char ',') *> parseItem)) <|> pure [])

parseItem :: Parser Item
parseItem = (Value <$> integer) <|> (parseList)

data TriBool = TFalse | TTrue | TUnknown

(|&) :: TriBool -> TriBool -> TriBool
(|&) TUnknown r = r
(|&) l _ = l

toBool :: TriBool -> Bool
toBool TTrue = True
toBool TFalse = False

comp :: Item -> Item -> TriBool
comp (Value l) (Value r) 
  | l < r     = TTrue
  | l > r     = TFalse
  | otherwise = TUnknown
comp (List l) (Value r) = comp (List l) (List [Value r])
comp (Value l) (List r) = comp (List [Value l]) (List r)
comp (List []) (List []) = TUnknown
comp (List []) r = TTrue
comp l (List []) = TFalse
comp (List (l:l')) (List (r:r')) = (comp l r) |& (comp (List l') (List r'))

