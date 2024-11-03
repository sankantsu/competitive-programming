import Text.Printf (printf)

readInts :: IO [Int]
readInts = map read . words <$> getLine

firstOrDefault :: a -> [a] -> a
firstOrDefault x [] = x
firstOrDefault x (y : _) = y

f :: Int -> Int -> Int -> Int
f i j k = 10000*i + 5000*j + 1000*k

main :: IO ()
main = do
  [n, y] <- readInts
  let l = [(i, j, k) | i <- [0..n],
                       j <- [0..(n - i)],
                       k <- [n - i -j],
                       f i j k == y]
  let (i, j, k) = firstOrDefault (-1, -1, -1) l
  putStrLn $ printf "%d %d %d" i j k
  return ()
