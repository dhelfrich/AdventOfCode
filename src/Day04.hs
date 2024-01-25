module Day04 (day04) where

import Crypto.Hash
import Data.ByteString.Char8 (pack)

day04 :: IO ()
day04 = do
    input <- readFile "./inputs/day04.txt"
    let answer1 = length $ takeWhile (< 5) $ map (computeLeadingZeros input) [1..]
    let answer2 = length $ takeWhile (< 6) $ map (computeLeadingZeros input) [1..]
    print $ "Part 1: " ++ show (answer1 + 1)
    print $ "Part 2: " ++ show (answer2 + 1)

computeLeadingZeros :: String -> Int -> Int
computeLeadingZeros key num = leadingZeros hash'
    where hash' = show $ hashWith MD5 $ pack $ key ++ show num
          leadingZeros :: String -> Int
          leadingZeros = length . takeWhile (== '0') 

