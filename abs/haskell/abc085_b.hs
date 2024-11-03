import Control.Monad (replicateM)
import Data.List (nub)

readInt :: IO Int
readInt = readLn

main :: IO ()
main = do
  n <- readInt
  d <- replicateM n readInt
  print $ length $ nub d
