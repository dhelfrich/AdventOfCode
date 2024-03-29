{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE GADTs #-}
module Y2015.Day06 (day06) where
import Text.Regex.TDFA ( (=~) )
import qualified Data.Vector.Mutable as V
import Data.Vector.Mutable (IOVector)
import Data.Foldable

type Coord = (Int, Int)
type Lights = IOVector Bool
type Lights2 = IOVector Int
data ActionType = On | Off | Toggle deriving (Eq, Show)
data Action where
  Action :: {
    actionType :: ActionType,
    corner1 :: Coord,
    corner2 :: Coord} ->
    Action
  deriving (Eq, Show)

day06 :: IO ()
day06 = do
  input <- readFile "./inputs/2015/day06.txt"
  -- putStr $ unlines $ take 10 $ map (show . parse) $ lines input
  l <- lightsOff
  l2 <- lightsOff2
  let actions = map parse $ lines input
  applyAllActions l actions
  applyAllActions2 l2 actions
  x1 <- V.foldl (\a x -> if x then (1 + a) :: Integer else a) 0 l
  x2 <- V.foldl (+) 0 l2
  print $ "Part 1: " ++ show x1
  print $ "Part 2: " ++ show x2


parse :: String -> Action
parse line =
  case matchType of
    "turn on"    -> Action On (x1, y1) (x2, y2)
    "turn off"   -> Action Off (x1, y1) (x2, y2)
    "toggle"     -> Action Toggle (x1, y1) (x2, y2)
    _ -> Action Toggle (0,0) (0,0)
  where
  [[_, matchType, x1', y1', x2', y2']] = line =~ pat
  [x1, y1, x2, y2] = map read [x1', y1', x2', y2']
  pat = "(turn on|turn off|toggle) ([0-9]+),([0-9]+) through ([0-9]+),([0-9]+)"

lightsOff :: IO (IOVector Bool)
lightsOff = do V.replicate 1000000 False


applyAction :: Lights -> Action -> IO ()
applyAction lights act = do
  case actionType act of
    On -> mapSquare (const True) ind
    Off -> mapSquare (const False) ind
    Toggle -> mapSquare not ind
  where
  (x1, y1) = corner1 act
  (x2, y2) = corner2 act
  mapSquare f ind' = for_ ind' (V.modify lights f)
  ind = [y * 1000 + x | x <- [x1 .. x2], y <- [y1..y2]]


applyAllActions :: Lights -> [Action] -> IO ()
applyAllActions lights (a:as) = do
  applyAction lights a
  applyAllActions lights as
applyAllActions _ [] = do
  pure ()

lightsOff2 :: IO (IOVector Int)
lightsOff2 = do V.replicate 1000000 0

applyAction2 :: Lights2 -> Action -> IO ()
applyAction2 lights act = do
  case actionType act of
    On -> mapSquare (+1) ind
    Off -> mapSquare (\x -> if x > 0 then x - 1 else 0) ind
    Toggle -> mapSquare (+2) ind
  where
  (x1, y1) = corner1 act
  (x2, y2) = corner2 act
  mapSquare f ind' = for_ ind' (V.modify lights f)
  ind = [y * 1000 + x | x <- [x1 .. x2], y <- [y1..y2]]

applyAllActions2 :: Lights2 -> [Action] -> IO ()
applyAllActions2 lights (a:as) = do
  applyAction2 lights a
  applyAllActions2 lights as
applyAllActions2 _ [] = do
  pure ()

