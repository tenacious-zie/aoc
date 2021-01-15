import Prelude

sol1 :: Int -> [Int] -> Maybe Int
sol1 _ [] = Nothing
sol1 n (x:xs) = if (n - x) `elem` xs then  Just (x * (n - x)) else sol1 n xs

sol2 :: [Int] -> Int
sol2 (x:xs) = case sol1 (2020 - x) xs of
                Nothing -> sol2 xs
                Just y  -> y * x

main :: IO ()
main = do
  xs <-  sol2  . map (\x -> read x :: Int) . lines <$> readFile "./data/day01.txt"
  putStrLn $ (show xs)
