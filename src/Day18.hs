{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Day18 (day18) where
import qualified Data.Map as M
import Data.List (delete, nub)
import GHC.Utils.Misc (count)


type Grid = M.Map (Int, Int) Bool

day18 :: IO ()
day18 = do
  input <- readFile "./inputs/day18.txt"
  let app100 x = iterate (next 99) x !! 100
      app100' x = iterate (cornersOn 99 . next 99) x !! 100
      p1str = (toStr 99 . app100 . toMap 99) input
      p1 = count (=='#') p1str
      p2str = (toStr 99 . app100' . cornersOn 99 . toMap 99) input
      p2 = count (=='#') p2str
  print $ "Part 1: " ++ show p1
  print $ "Part 2: " ++ show p2

next :: Int -> Grid -> Grid
next size g = M.mapWithKey f g
  where
  isOnNext n c = (c && (n == 2 || n == 3)) || (not c && (n == 3))
  f k current = isOnNext neighborsOn current
    where
    neighborsOn = count (g M.!) neighbors
    (i, j) = k
    neighbors = delete (i, j) [(i', j') | i' <- neighIs, j' <- neighJs]
    neighIs = nub [i, max (i - 1) 0, min (i + 1) size]
    neighJs = nub [j, max (j - 1) 0, min (j + 1) size]

cornersOn :: Int -> Grid -> Grid
cornersOn size = M.union cornOn
  where cornOn = M.fromList [((i, j), True) | i <- [0, size], j <- [0, size]]

toMap :: Int -> String -> Grid
toMap size str = M.fromList $ zip indices str'
  where
  indices = [(i, j) | i <- [0..size], j <- [0..size]]
  str' = map (=='#') (filter (/='\n') str)

toStr :: Int -> Grid -> String
toStr size grid = unlines str
  where
  str = [[toChar (grid M.! (i, j)) | j <- [0..size]] | i <- [0..size]]
  toChar x = if x then '#' else '.'