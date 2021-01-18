module Day02 where

import Prelude
import Text.Parsec


rights (Right x) = x


p :: Parsec String () (Int, Int, Char, String)
p = do
  low    <- many1 digit
  string "-"
  high   <- many1 digit
  char ' '
  lter <- letter
  string ": "
  wd <- many1 letter
  return $ (read low, read high, lter, wd )

fw :: [String] -> Int
fw lst = let xs = map rights $ map (\x -> parse p "" x) lst in
  length $ filter f2 xs -- filter with f for part 1

f :: (Int, Int, Char, String) -> Bool
f (l, h, c, s) =
  let xs = length $ filter (\x -> x == c) s in
  if xs >= l && xs <= h then True else False


f2 :: (Int, Int, Char, String) -> Bool
f2 (l, h, c, s)= let p1 = s !! (l - 1)
                     p2 = s !! (h - 1)
                     in (c == p1) /= (c == p2)


main = do
  xs <- lines <$> readFile "./data/day02.txt"
  putStrLn $ show $ fw xs
