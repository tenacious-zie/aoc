import Prelude

sol1 :: [Int] -> Int
sol1 [] = 1
sol1 (x:xs) = if (2020 - x) `elem` xs then (x * (2020 - x)) else sol1 xs

main :: IO ()
main = do
  xs <-  sol1  . map (\x -> read x :: Int) . lines <$> readFile "./data/day01.txt"
  putStrLn $ (show xs)
