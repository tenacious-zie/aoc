{-# LANGUAGE ScopedTypeVariables#-}
import Text.Parsec
import Data.Char
-- shit

type Parser = Parsec String ()


stringi :: String -> Parser String
stringi = mapM chari


chari :: Char -> Parser Char
chari c = oneOf [toLower c, toUpper c]

data Instruction = Acc | Nop | Jmp deriving (Show, Eq, Read, Bounded, Enum)


p :: Parser (Instruction, Int)
p = do
  instr <- enump
  space
  sgn <- choice [char '+' >> return 1, char '-' >> return (-1), return 1]
  arg <- read <$> many1 digit
  return $ (instr, sgn * arg)



enump :: forall b. (Enum b, Bounded b, Show b) => Parser b
enump = choice $ map sr [minBound :: b .. maxBound :: b]
  where
    sr :: (Show b) => b -> Parser b
    sr x = try $ stringi (show x) >> return x


main :: IO ()
main = do
  xs <- lines <$> readFile "./data/day08.txt"
  print $ map (parse p "") xs
