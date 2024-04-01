module Y2016.Day06 (day06) where
import Data.List (sortOn, group, sort)
import Debug.Trace (trace)

day06 :: IO ()
day06 = do
  input <- readFile "./inputs/2016/day06.txt"
  let t = transpose . lines $ input
      p1 = head . last . sortOn length . group . sort <$> t
      p2 = head . head . sortOn length . group . sort <$> t
  print $ "Part 1: " ++ p1
  print $ "Part 2: " ++ p2

transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose x = map head x : transpose (map tail x)

  
