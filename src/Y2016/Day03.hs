module Y2016.Day03 (day03) where
import Data.List (sort)

day03 :: IO ()
day03 = do
  input <- readFile "./inputs/2016/day03.txt"
  let p1 = length $ filter testTri  (sort <$> pInput input)
      newInp = group3 . concat . transpose $ pInput input
      p2 = length $ filter testTri (sort <$> newInp)
  print $ "Part 1: " ++ show p1
  print $ "Part 2: " ++ show p2

pInput :: String -> [[Int]]
pInput inp = (read <$>) . words <$> lines inp

transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

group3 :: [Int] -> [[Int]]
group3 = helper []
  where
  helper l [] = l
  helper l (a:b:c:xs) = helper (l ++ [[a, b, c]]) xs
  helper _ _ = error "Not groupable by 3"

testTri :: [Int] -> Bool
testTri [a, b, c] = a + b > c 
testTri _ = error "Not a triangle"