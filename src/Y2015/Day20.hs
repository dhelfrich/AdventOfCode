module Y2015.Day20 (day20) where
import Data.List (findIndex)

--This was more of a math problem, so it doesn't make sense to come up with a 
--general solution. These are my attempts, though.
day20 :: IO ()
day20 = do
  -- input <- readFile "./inputs/2015/day20.txt"
  let p1 = (*5040) <$> findIndex (>= 3310000) [numPresents' t | t <- [0,5040..]]
  let p2 = (*5040) <$> findIndex (>= 3009091) [numPresents50 t | t <- [0,5040..]]
  print $ "Part 1: " ++ show p1
  print $ "Part 2: " ++ show p2


isFactorOf :: Integral a => a -> a -> Bool
isFactorOf x n = n `mod` x == 0

factorList :: Integral a => a -> [a]
factorList n = [ x | x <- [1 .. n`div` 2] ++ [n], x `isFactorOf` n]


numPresents' :: Int -> Int
numPresents' 0 = 0
numPresents' n = (sum $ factorList n)

-- numPresents :: Int -> Int
-- numPresents n = 10 * numPresents' n

-- minHouse' :: Int -> Int
-- minHouse' n
--   | n < 100000000 = fromMaybe 0 $ findIndex (>= n) [numPresents' t | t <- [0..]]
  -- | otherwise =
  --   trace ("Calling f " ++ show n) $
  --     inc * fromMaybe 0 (findIndex (> n) [numPresents' t | t <- [0, inc..]])
  --       where inc = minHouse' (n `div` 2)

factorList50 :: Integral a => a -> [a]
factorList50 n = [ x | x <- [1 .. n`div` 2] ++ [n], x `isFactorOf` n, x > n `div` 50]

numPresents50 :: Int -> Int
numPresents50 0 = 0
numPresents50 n = (sum $ factorList50 n)
-- elf :: Int -> Int -> V.Vector Int
-- elf l n = V.generate (l + 1) (\x -> if x `mod` n == 0 && n /= 0 then 10*n else 0)

-- numPresents :: Int -> Int
-- numPresents n = pres V.! n
--   where
--   pres = foldl (V.zipWith (+)) (V.replicate (n + 1) 0) elfList
--   elfList = [elf n t | t <- [1..n]]
