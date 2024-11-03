import Data.List

readInt :: IO Int = readLn
readInts :: IO [Int] = map read . words <$> getLine

points :: [Int] -> (Int, Int)
points l = inner 0 0 0 l
  where
    inner turn x y [] = (x, y)
    inner turn x y (p : tail) =
      if even turn then
        inner (turn + 1) (x + p) y tail
      else
        inner (turn + 1) x (y + p) tail

main :: IO ()
main = do
  _n <- readInt
  a <- readInts
  let (x, y) = points $ reverse $ sort a
  print $ x - y
