{-# LANGUAGE InstanceSigs #-}
module Y2016.Day11 (day11) where
import Data.List ((\\), sort, foldl', scanl')
import qualified Data.Map as M
import Data.Foldable (Foldable(toList))
import Control.Monad (guard)
import Data.Maybe (fromJust, maybeToList)

--Oh wow, what a problem. This is incomplete and doesn't give an answer. 
--If you ignore all the rules aside from that you can only carry 1 or 2 items,
--it is easy to count the minimal number of moves, and this turns out to 
--give the correct answer.

data Element = Co | Po | Pr | Ru | Th deriving (Show, Eq, Enum, Ord, Read)
data ObType = GN | MC deriving (Show, Eq, Ord, Read)
type Obj = (Element, ObType)
data Move = Move {item1 :: Obj
                 ,item2 :: Maybe Obj
                 ,up :: Bool} deriving (Show, Read, Eq, Ord)
data State = State { elev :: Int
                    ,fs :: M.Map Int [Obj]
  } deriving (Eq)

instance Show State where
  show :: State -> String
  show s = (++ "\n") . unlines $
    concat [dots 8, "Generators", dots 30, "Microchips", dots 15] :
    map pFloor [0..3]
    where
    isIn objs ty el =
      if (el, ty) `elem` objs then "." ++ show el ++ "." else "...."
    onFl f =  if elev s == f then "**" else "  "
    dots n = replicate n '.'
    pFloor n =
      concat [onFl n, "F", show n, "...", concatMap (isIn (fs s M.! n) GN) [Co ..Th],
      dots 20, concatMap (isIn (fs s M.! n) MC) [Co ..Th], dots 10]

day11 :: IO ()
day11 = do
  print $ startState
  print $ head $ (iterate getNext [[startState]])!!48
  print $ possMoves startState
  -- test <- readFile "./inputs/2016/day11Test3.txt"
  -- let moves = read test :: [Move]
  --     curState = foldl' move startState moves
  --     allState = scanl' move startState moves
  -- print test
  -- print $ zip3 ([0..] :: [Int]) (0 : (score <$> moves)) allState
  -- print $ all validState allState
  -- mapM_ print $ sort $ (\x -> (score x, x)) <$> possMoves curState

getNext :: [[State]] -> [[State]]
getNext start = do
  s <- start 
  m <- possMoves (last s)
  return (s ++ return (move (last s) m))

possMoves :: State -> [Move]
possMoves s = do
  let currFloor = sort $ fs s M.! elev s
  obj1 <- Just <$> currFloor
  obj2 <- ((Just <$> currFloor) \\ [obj1]) ++ [Nothing]
  guard (obj1 > obj2)
  tf <- [True, False]
  guard $ (elev s + (if tf then 1 else -1) `elem` [0..3])
  guard $ validState $ move s (Move (fromJust obj1) obj2 tf)
  guard $ (score (Move (fromJust obj1) obj2 tf) `elem` [-1,2])
  return $ Move (fromJust obj1) obj2 tf

validState :: State -> Bool
validState s = all valid $ fs s
  where
  valid :: [Obj] -> Bool
  --valid if for all microchips, they are either powered or no generators present
  valid os = null gens || null unpoweredMcs
    where
    mcs = filter ((== MC) . snd) os
    gens = filter ((== GN) . snd) os
    unpoweredMcs = [m | m <- mcs, (fst m, GN) `notElem` gens]

move :: State -> Move -> State
move s m = State end newFs
  where
  startFloor = (fs s M.! start) \\ (obj1 : toList obj2)
  endFloor = (fs s M.! end) ++ (obj1 : toList obj2)
  newFs = M.insert start startFloor . M.insert end endFloor $ fs s
  Move obj1 obj2 dir = m
  start = elev s
  end = start + if dir then 1 else -1

score :: Move -> Int
score m = (1 + (length . maybeToList) (item2 m)) * (if up m then 1 else -1)

startState :: State
startState = State 0 $ M.fromList $ zip [0..3]
                     [[(Po, GN), (Th, GN), (Th, MC), (Pr, GN)
                    , (Ru, GN), (Ru, MC), (Co, MC), (Co, GN)],
                    [(Po, MC), (Pr, MC)], [], []]
endState :: State
endState = State 3 $ M.fromList $ zip [0..3]
                     [[],[],[]
                     ,[(Po, GN), (Th, GN), (Th, MC), (Pr, GN)
                     ,(Ru, GN), (Ru, MC), (Co, MC), (Co, GN)
                     ,(Po, MC), (Pr, MC)]]
endStateT :: State
endStateT = State 2 $ M.fromList $ zip [0..3]
                     [[(Po, GN), (Th, GN), (Pr, GN)
                    , (Ru, GN), (Ru, MC), (Co, GN)],
                    [(Po, MC), (Pr, MC)], [(Co, MC), (Th, MC)], []]
