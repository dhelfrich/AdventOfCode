module Y2015.Day25 (day25) where

day25 :: IO ()
day25 = do
  let input = (2947, 3029)
      entry = uncurry getEntry input
      p1 = getCode entry
  print $ "Part 1: " ++ show p1

getEntry :: Integer -> Integer -> Integer
getEntry row 1 = (((row - 1) * row) `div` 2) + 1
getEntry row col = getEntry (row + 1) (col - 1) + 1

--20151125 * 252533 ^ (n - 1) `mod` 33554393
getCode :: Integer -> Integer
getCode 1 = 20151125
getCode entry = (getCode (entry - 1) * 252533) `mod` 33554393