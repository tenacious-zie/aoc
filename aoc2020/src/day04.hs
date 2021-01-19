import A
import Data.Map as M hiding(filter, map)
data FT = Byr | Iyr | Eyr | Hgt | Hcl | Ecl | Pid | Cid
  deriving (Show, Eq, Ord, Enum, Bounded)

fieldtype :: Parser FT
fieldtype = try (string "byr" >> return Byr)
            <|> try (string "iyr" >> return Iyr)
            <|> try (string "eyr" >> return Eyr)
            <|> try (string "hgt" >> return Hgt)
            <|> try (string "hcl" >> return Hcl)
            <|> try (string "ecl" >> return Ecl)
            <|> try (string "pid" >> return Pid)
            <|> try (string "cid" >> return Cid)

field :: Parser (FT, String)
field = do
  ft <- fieldtype
  char ':'
  s <- manyTill anyChar space
  return (ft, s)

p :: Parser [[(FT, String)]]
p = many1 $ do
  t <- many1 field
  (char '\n' >> return ()) <|> eof
  return t


f (Right ps) = length  . filter (== 7) $ map (length . M.delete Cid . M.fromList) ps
f (Left ps) = 5
main :: IO ()
main = do
  xs <- readFile "./data/day04.txt"
  print $ f $ parse p xs

