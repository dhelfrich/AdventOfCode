module Y2016.Day02 (day02) where
import qualified Data.Map as M

day02 :: IO ()
day02 = do
  input <- readFile "./inputs/2016/day02.txt"
  let dirs = pInput input
      p1 = getCode dirs
      p2 = getCode2 dirs
  print $ "Part 1: " ++ p1
  print $ "Part 2: " ++ p2

data Dir = U | D | L | R deriving (Show)
type Pos = (Int, Int)

--So this function uses the top left as (0,0) and uses i, j like a matrix
getCode :: [[Dir]] -> String
getCode dirs = tail $ posToNum <$> scanl (foldl mover) (1,1) dirs
  where 
  mover :: Pos -> Dir -> Pos
  mover (i, j) d = case d of
    U -> (max 0 (i - 1), j)
    D -> (min 2 (i + 1), j)
    L -> (i, max 0 (j - 1))
    R -> (i, min 2 (j + 1))

--This one uses the middle as (0,0), and x, y as coords
getCode2 :: [[Dir]] -> String
getCode2 dirs = tail $ posToNum2 <$> scanl (foldl mover2) (-2, 0) dirs

mover2 :: Pos -> Dir -> Pos 
mover2 (x, y) d
  | abs x' + abs y' <= 2 = (x', y')
  | otherwise = (x, y)
      where 
      (x', y') = case d of 
        U -> (x, y + 1)
        D -> (x, y - 1)
        L -> (x - 1, y)
        R -> (x + 1, y)

posToNum :: Pos -> Char
posToNum (i, j) = head $ show (i * 3 + j + 1)

posToNum2 :: Pos -> Char
posToNum2 pos = matrix M.! pos
  where
  matrix = M.fromList $ zip coords nums
  coords = [(0, 2), (-1, 1), (0, 1), (1, 1), (-2, 0), (-1, 0), 
            (0, 0), (1, 0), (2, 0), (-1, -1), (0, -1), (1, -1), (0, -2)]
  nums = ['1', '2', '3', '4', '5', '6', '7', '8', '9', 'A', 'B', 'C', 'D']

pInput :: String -> [[Dir]]
pInput inp = map (pDur <$>) (lines inp)

pDur :: Char -> Dir
pDur c = case c of
  'U' -> U
  'D' -> D
  'L' -> L
  'R' -> R
  _ -> error "not a direction"