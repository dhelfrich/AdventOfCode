{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Y2015.Day14 (day14, points) where
import Text.Megaparsec
    ( anySingleBut,
      noneOf,
      parse,
      errorBundlePretty,
      many,
      some,
      Parsec )
import Text.Megaparsec.Char ( alphaNumChar, space )
import Data.Text (Text, pack)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void ( Void )
import Data.List ( elemIndices, foldl' )
import qualified Data.Map as Map

type Parser = Parsec Void Text
data Reindeer = Reindeer {
  name :: String,
  speed :: Int,
  duration :: Int,
  rest :: Int} deriving Show

day14 :: IO ()
day14 = do
  -- input <- getLine
  input <- readFile "./inputs/2015/day14.txt"
  case parse (some pReindeer) "" (pack input) of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right xs -> do
      print xs
      print $ "Part 1: " ++ show (maximum (map (`distance` 2503) xs))
      print $ "Part 2: " ++ show (totalPoints xs 2503)

distance :: Reindeer -> Int -> Int
distance rein time = (time `div` cycleTime) * (speed rein * duration rein) +
                     min (time `mod` cycleTime) (duration rein) * speed rein
                     where cycleTime = duration rein + rest rein

totalPoints :: [Reindeer] -> Int -> Map.Map Int Int
totalPoints reins time = foldl' increasePoints (Map.fromList (zip [0..8] [0,0..])) indices
  where increasePoints :: Map.Map Int Int -> Int -> Map.Map Int Int
        increasePoints oldMap index = Map.insertWith (+) index 1 oldMap
        indices = [1..time] >>= points reins

points :: [Reindeer] -> Int -> [Int]
points reins time = elemIndices best distances
  where distances = map (`distance` time) reins
        best = maximum distances

pReindeer :: Parser Reindeer
pReindeer = do
  _ <- space
  name' <- some alphaNumChar
  speed' <- many (noneOf ['0'..'9']) *> L.decimal
  duration' <- many (noneOf ['0'..'9']) *> L.decimal
  rest' <- many (noneOf ['0'..'9']) *> L.decimal
  _ <- many (anySingleBut '\n') *> space
  return Reindeer {name = name'
                  ,speed = speed'
                  ,duration = duration'
                  ,rest = rest'}
