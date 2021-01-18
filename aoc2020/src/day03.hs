module Day03 where
import Prelude

f :: [String] -> [[Bool]]
f = map (cycle . map (== '#'))

c :: [[Bool]] -> Int ->  Int ->  Int -> Int -> [Bool]
c (x:xs) m v w j = (x !! m): c xs (m + v) v w w
c (x:xs) m v w j = c xs (m + v) v w (j - 1)
c _ _ _ _ _ = []

main = do
  xs <- lines <$> readFile "./data/day03.txt"
  print $ length $ filter (== True) $ c (f xs) 0 1 0 0
  print $ length $ filter (== True) $ c (f xs) 0 3 0 0
  print $ length $ filter (== True) $ c (f xs) 0 5 0 0
  print $ length $ filter (== True) $ c (f xs) 0 7 0 0
  print $ length $ filter (== True) $ c (f xs) 0 1 1 2
