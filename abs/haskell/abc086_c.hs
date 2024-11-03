import Control.Monad (replicateM)

readInt :: IO Int
readInt = readLn

toTxy :: [Int] -> (Int, Int, Int)
toTxy [t, x, y] = (t, x, y)

readTxy :: IO (Int, Int, Int)
readTxy = toTxy . map read . words <$> getLine

reachable :: Int -> (Int, Int) -> (Int, Int) -> Bool
reachable t (x0, y0) (x1, y1) =
  let d = abs (x1 - x0) + abs (y1 - y0) in
  d <= t && (odd d == odd t)

solve :: [(Int, Int, Int)] -> Bool
solve = inner 0 0 0
  where
    inner _ _ _ [] = True
    inner t x y ((t1, x1, y1) : rest) =
      reachable (t1 - t) (x, y) (x1, y1)
      && inner t1 x1 y1 rest

main :: IO ()
main = do
  n <- readInt
  txy <- replicateM n readTxy
  putStrLn $ if solve txy then "Yes" else "No"
