module Y2015.Day24 (day24) where
import Data.List ( tails, (\\), sortOn )
import Control.Monad (guard)

--For part 2 we really need to double check that there exists an even 3-partition
--of the remaining bits, but it is very likely that it does exist. Also, for 
--part 1, by trial and error, we easily determined that 6 was the smallest set
--size that would give us a valid solution. For part 2, that was 5.
day24 :: IO ()
day24 = do
  input <- readFile "./inputs/2015/day24.txt"
  let p1 = snd . head $ sortOn snd (partition3 6 (parse input))
      combos = combinations 5 (parse input)
      p2 = product . head $ sortOn product $ filter ((==381).sum) combos
  print $ "Part 1: " ++ show p1
  print $ "Part 2: " ++ show p2

parse :: String -> [Int]
parse s = read <$> lines s

--https://stackoverflow.com/questions/127704/algorithm-to-return-all-combinations-of-k-elements-from-n/8626006#8626006
combinations :: Int -> [Int] -> [[Int]]
combinations 0 _ = [[]]
combinations n lst = do
    (x:xs) <- tails lst
    rest   <- combinations (n-1) xs
    return $ x : rest

--https://stackoverflow.com/questions/32575630/powerset-of-a-set-with-list-comprehension-in-haskell
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

partition3 :: Int -> [Int] -> [([Int], Int)]
partition3 n lst = do
  comb <- filter ((==(sum lst `div` 3)).sum) (combinations n lst)
  guard $ any ((==(sum lst `div` 3)).sum) (powerset (lst \\ comb))
  return (comb, product comb)