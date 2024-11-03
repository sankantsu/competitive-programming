digitSum :: Int -> Int
digitSum n
  | n < 10 = n
  | otherwise = (n `mod` 10) + (digitSum (n `div` 10))

main :: IO ()
main = do
  [n, a, b] <- map (read :: String -> Int) . words <$> getLine
  let lst = [(k, digitSum k) | k <- [1..n]]
  print $ foldl (+) 0 $ map (\(x,s) -> x) $ filter (\(x,s) -> a <= s && s <= b) lst
