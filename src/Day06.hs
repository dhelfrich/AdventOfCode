{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE GADTs #-}
module Day06 (day06) where
import Text.Regex.TDFA ( (=~) )
import qualified Data.Vector.Mutable as V
import Data.Vector.Mutable (IOVector)
import Data.Foldable

type Coord = (Int, Int)
type Lights = IOVector Bool
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
  input <- readFile "./inputs/Day06.txt"
  -- putStr $ unlines $ take 10 $ map (show . parse) $ lines input
  l <- lightsOff
  let actions = map parse $ lines input
  applyAllActions l actions
  x <- V.foldl (\a x -> if x then a + 1 else a) 0 l
  print x


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

-- applyAction2 :: Lights -> Action -> IO ()
-- applyAction2 lights act = do
--   case actionType act of
--     On -> mapSquare (+1) ind
--     Off -> mapSquare (-1) ind
--     Toggle -> mapSquare (+2) ind
--   where
--   (x1, y1) = corner1 act
--   (x2, y2) = corner2 act
--   mapSquare f ind' = for_ ind' (V.modify lights f)
--   ind = [y * 1000 + x | x <- [x1 .. x2], y <- [y1..y2]]

-- applyAllActions2 :: Lights -> [Action] -> IO ()
-- applyAllActions2 lights (a:as) = do
--   applyAction2 lights a
--   applyAllActions2 lights as
-- applyAllActions2 _ [] = do
--   pure ()

