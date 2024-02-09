module Day09 (day09) where
import Text.Regex.TDFA ( (=~) )
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import Data.List (nub, permutations)
type DistsMap = M.Map (String, String) Int

day09 :: IO ()
day09 = do
  input <- readFile "./inputs/Day09.txt"
  let part1 = findShortestDistance $ parseLocations input
  let part2 = findLongestDistance $ parseLocations input
  print $ unwords ["Part 1: ", show part1]
  print $ unwords ["Part 2: ", show part2]

parseLocations :: String -> M.Map (String, String) Int
parseLocations s = M.fromList pairsDoubled
  where 
    pairs = mapMaybe parseStr $ lines s
    pairsDoubled = pairs >>= reverse'
    reverse' ((loc1, loc2), dist) = [((loc1, loc2), dist),((loc2, loc1), dist)]


parseStr :: String -> Maybe ((String, String), Int)
parseStr s = case m of 
  [[_, loc1, loc2, dist']] -> Just ((loc1, loc2), dist)
    where dist = read dist'
  _ -> Nothing
  where m = (s =~ "([A-Za-z]+) to ([A-Za-z]+) = ([0-9]+)") :: [[String]]

findShortestDistance :: DistsMap -> Int
findShortestDistance dmap =
  minimum (mapMaybe (calcDistance dmap) allPaths)
  where 
  allPaths = permutations $ nub $ map (fst . fst) $ M.toList dmap

findLongestDistance :: DistsMap -> Int
findLongestDistance dmap =
  maximum (mapMaybe (calcDistance dmap) allPaths)
  where 
  allPaths = permutations $ nub $ map (fst . fst) $ M.toList dmap

calcDistance :: DistsMap -> [String] -> Maybe Int
calcDistance dmap (x1:x2:xs) = (+) <$>  (Just $ dmap M.! (x1, x2)) <*> calcDistance dmap (x2:xs)
calcDistance _ [_] = Just 0
calcDistance _ [] = Just 0