module Day01
    (day01

    )where

import Data.List ( inits )

day01 :: IO ()
day01 = do
    input <- readFile "./inputs/day01.txt"
    print $ "Part 1: " ++ show (getFloor input)
    print $ "Part 2: " ++ show (answer2 input)
    where
        answer2 input = length $ takeWhile (>=0) (map getFloor $ inits input)

getFloor :: String -> Int
getFloor a = sum (map values a)
    where
        values '(' = 1
        values ')' = -1
        values _ = 0

