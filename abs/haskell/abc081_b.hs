solve :: [Int] -> Int
solve l =
  if all even l then
    1 + (solve $ map (\x -> div x 2) l)
  else
    0

main :: IO ()
main = do
  _n :: Int <- read <$> getLine
  l :: [Int] <- map read . words <$> getLine
  print $ solve l
