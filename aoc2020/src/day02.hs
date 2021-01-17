module Day02 where

import Prelude
import Text.Parsec


rights (Right x) = x
rights (Left x) = "3"

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

main = do
  xs <- lines <$> readFile "./data/day02.txt"
  putStrLn $ rights $ parse p "" (head xs)
