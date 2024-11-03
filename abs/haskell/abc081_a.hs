toInts :: String -> [Int]
toInts s = map (read . pure) s

main :: IO ()
main = do
  s <- getLine
  let ints = toInts s
  let ans = foldl (+) 0 ints
  print ans
