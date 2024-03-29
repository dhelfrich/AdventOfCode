{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Y2015.Day17 (day17) where

day17 :: IO ()
day17 = do
  input <- readFile "./inputs/2015/day17.txt"
  let containers = map read $ lines input
      p1 = countSums 150 containers
      minContainers = getMinContainers 150 containers
      p2 = countSums2 150 minContainers containers
  print $ "Part 1: " ++ show p1
  print $ "Part 2: " ++ show p2

countSums :: Int -> [Int] -> Int
countSums total conts = length combos
  where
    combos = filter (\x -> sum x == total) $ sequence conts'
    conts' = (\x -> [0, x]) <$> conts

getMinContainers :: Int -> [Int] -> Int
getMinContainers total conts = minimum $ map length combos'
  where
    combos' = map (filter (/=0)) combos
    combos = filter (\x -> sum x == total) $ sequence conts'
    conts' = (\x -> [0, x]) <$> conts

countSums2 :: Int -> Int -> [Int] -> Int
countSums2 total numConts conts = length combos''
  where
    combos'' = filter (\x -> length x == numConts) combos'
    combos' = map (filter (/=0)) combos
    combos = filter (\x -> sum x == total) $ sequence conts'
    conts' = (\x -> [0, x]) <$> conts