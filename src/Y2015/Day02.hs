module Y2015.Day02
    (day02

    )where


import Data.List.Split (splitOn)

day02 :: IO ()
day02 = do
    input <- readFile "./inputs/2015/day02.txt"
    let answer1 = sum $ map (getWrappingPaperArea . splitOn "x") $ lines input
    let answer2 = sum $ map (getRibbonLength . splitOn "x") $ lines input
    print $ "Part 1: " ++ show answer1
    print $ "Part 2: " ++ show answer2


getWrappingPaperArea :: [String] -> Int
getWrappingPaperArea xs = case map read xs of
                        [a,b,c] -> 2 * (a * b + a * c + b * c) + min'
                            where min' = minimum [a*b, a*c, b*c]
                        _ -> 0

getRibbonLength :: [String] -> Int
getRibbonLength xs = case map read xs of
                        [a,b,c] -> a * b * c + 2 * min'
                            where min' = minimum [a+b, a+c, b+c]
                        _ -> 0


