module Y2015.Day10 (day10) where
import Data.List (group)


day10 :: IO ()
day10 = do
  input <- readFile "./inputs/2015/day10.txt"
  let part1 = (map length (iterate readNums input)) !! 50
  print $ unwords ["Part 1: ", show part1]

readNums :: String -> String
readNums l = concatMap toNumbers (group l)
  where
  toNumbers :: String -> String
  toNumbers x = (show . length) x ++ [head x]
