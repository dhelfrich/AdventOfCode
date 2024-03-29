{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Y2015.Day03
    (day03

    )where

import Data.List (delete)

day03 :: IO ()
day03 = do
    input <- readFile "./inputs/2015/day03.txt"
    print $ "Part 1: " ++ show (getNumberHouses input)
    print $ "Part 1: " ++ show (getNumberHouses2 input)


getNumberHouses :: [Char] -> Int
getNumberHouses str = length houseList
    where houseList = foldl helper [(0,0)] str

helper :: [(Int, Int)] -> Char -> [(Int, Int)]
helper acc char = if x' `elem` acc then x': delete x' acc else x': acc
                    where x:_ = acc
                          x' = case char of
                            '^' -> (i, j+1)
                            'v' -> (i, j-1)
                            '<' -> (i-1, j)
                            '>' -> (i+1, j)
                            _ -> (i, j)
                            where (i, j) = x

splitEvenOdd :: [Char] -> ([Char], [Char])
splitEvenOdd (x1:x2:xs) = (x1:odds, x2:evens)
    where (odds, evens) = splitEvenOdd xs
splitEvenOdd [x1] = ([x1], [])
splitEvenOdd _ = ([], [])

getNumberHouses2 :: [Char] -> Int
getNumberHouses2 str = length houseList
    where houseList = merge houseList1 houseList2
          houseList1 = foldl helper [(0,0)] str1
          houseList2 = foldl helper [(0,0)] str2
          (str1, str2) = splitEvenOdd str
          merge (x:xs) list = merge xs $ if x `elem` list then list else x:list
          merge [] list = list
