import Data.Maybe (mapMaybe, listToMaybe)

l :: [String]
l = ["dream", "dreamer", "erase", "eraser"]

removePrefix :: String -> String -> Maybe String
removePrefix [] s = Just s
removePrefix (x:xs) (y:ys)
  | x == y = removePrefix xs ys
removePrefix _ _ = Nothing

solve :: String -> Bool
solve "" = True
solve s = maybe False solve . listToMaybe $ mapMaybe (\t -> removePrefix (reverse t) s) l

main :: IO ()
main = do
  s <- getLine
  let t :: String = reverse s
  let ans = if solve t then "YES" else "NO"
  putStrLn ans
