import A
import Data.Foldable
import Data.List
f = foldl' (\x y -> x*2 + y) 0 . map (bin )

bin 'F' = 0
bin 'B' = 1
bin 'L' = 0
bin 'R' = 1
main :: IO ()
main = do
  xs <- lines <$> readFile "./data/day05.txt"
  print $ maximum $ map f xs
  print $ g  . sort $ map f xs 

g xs = head [ succ x | (x, y) <- zip xs (tail xs), succ x /= y]
