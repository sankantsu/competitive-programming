readInt :: IO Int = readLn

main :: IO ()
main = do
  a <- readInt
  b <- readInt
  c <- readInt
  x <- readInt
  let yens = [500*i + 100*j + 50*k | i <- [0..a], j <- [0..b], k <- [0..c]]
  print $ length $ filter (== x) yens
