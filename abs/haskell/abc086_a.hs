ints :: IO [Int]
ints = map read . words <$> getLine

main :: IO ()
main = do
  [a, b] <- ints
  let result = if odd (a * b) then "Odd" else "Even"
  putStrLn result
