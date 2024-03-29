{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Y2015.Day15 (day15) where
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Control.Monad (guard)

type Parser = Parsec Void Text
data Ingredient = Ingredient {
  name :: String,
  cap :: Int,
  dur :: Int,
  flav :: Int,
  tex :: Int,
  cal :: Int
} deriving (Show, Eq)

day15 :: IO ()
day15 = do
  -- input <- getLine
  input <- readFile "./inputs/2015/day15.txt"
  case parse (some pIngredient) "" (pack input) of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right xs -> do
      let best1 = maximum $ calcScore xs <$> proportions 100
      let best2 = maximum $ calcScore xs <$> proportions2 xs 500
      print $ "Part 1: " ++ show best1
      print $ "Part 2: " ++ show best2
      -- print $ sort l

calcScore :: [Ingredient] -> [Int] -> Int
calcScore ingredients portions = product (map subscore props)
  where
  subscore prop = max 0 $ sum (zipWith (*) (prop <$> ingredients) portions)
  props = [cap, dur, flav, tex]

proportions :: Int -> [[Int]]
proportions tot = [[a, b, c, d]
                  | a <- [0..tot], b <- [0..tot - a],
                    c <- [0..tot-a-b], d <- [0..tot - a - b - c]]

proportions2 :: [Ingredient] -> Int -> [[Int]]
proportions2 ing calTotal = do
  let maxList = map (\x -> calTotal `div` cal x) ing
  a <- [0..maxList !! 0]
  b <- [0..maxList !! 1]
  c <- [0..maxList !! 2]
  d <- [0..maxList !! 3]
  guard (sum (zipWith (*) [a, b, c, d] (cal <$> ing)) == calTotal)
  guard (sum [a, b, c, d] == 100)
  pure [a, b, c, d]

pIngredient :: Parser Ingredient
pIngredient = do
  _ <- space
  name' <- some alphaNumChar
  cap' <- chunk ": capacity " *> L.signed space L.decimal
  dur' <- chunk ", durability " *> L.signed space L.decimal
  flav' <- chunk ", flavor " *> L.signed space L.decimal
  tex' <- chunk ", texture " *> L.signed space L.decimal
  cal' <- chunk ", calories " *> L.signed space L.decimal
  return Ingredient {name = name', cap = cap', dur = dur',
  flav = flav', tex = tex', cal = cal'}
