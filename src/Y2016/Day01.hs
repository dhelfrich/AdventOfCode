{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Y2016.Day01 (day01) where
import Text.Megaparsec
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec.Char (space, char)
import Control.Monad (void)
import qualified Text.Megaparsec.Char.Lexer as L


data Direction = L Int | R Int | Str deriving (Show)
data Pointed = N | E | S | W deriving (Show)
data PlayerPos = Pos (Int, Int) Pointed deriving (Show)
type Parser = Parsec Void Text

day01 :: IO ()
day01 = do
  input <- readFile "./inputs/2016/day01.txt"
  case parse pInput "" (pack input) of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right result -> do
      let Pos (x, y) _ = foldl takeDir (Pos (0, 0) N) result
          p1 = abs x + abs y
          newDir = mconcat $ map expandDir result
          positions = scanl takeDir (Pos (0, 0) N) newDir
          p2 = case firstRep (map getPos positions) of
            Just (x', y') -> abs x' + abs y'
            Nothing -> 0
      print $ "Part 1: " ++ show p1
      print $ "Part 2: " ++ show p2


takeDir :: PlayerPos -> Direction -> PlayerPos
takeDir (Pos (x, y) facing) dir = case facing of
  N -> case dir of
    L n -> Pos (x - n, y) W
    R n -> Pos (x + n, y) E
    Str -> Pos (x, y + 1) N
  E -> case dir of
    L n -> Pos (x, y + n) N
    R n -> Pos (x, y - n) S
    Str -> Pos (x + 1, y) E
  S -> case dir of
    L n -> Pos (x + n, y) E
    R n -> Pos (x - n, y) W
    Str -> Pos (x, y - 1) S
  W -> case dir of
    L n -> Pos (x, y - n) S
    R n -> Pos (x, y + n) N
    Str -> Pos (x - 1, y) W

expandDir :: Direction -> [Direction]
expandDir (L n) = L 1 : replicate (n - 1) (Str)
expandDir (R n) = R 1 : replicate (n - 1) (Str)
expandDir Str = [Str]

firstRep :: Eq a => [a] -> Maybe a
firstRep l = firstRep' l []
  where
  firstRep' :: Eq a => [a] -> [a] -> Maybe a
  firstRep' [] _ = Nothing
  firstRep' (x:xs) set = 
    if x `elem` set then Just x
    else firstRep' xs (x:set)

getPos :: PlayerPos -> (Int, Int)
getPos (Pos (x, y) _) = (x,y)

pInput :: Parser [Direction]
pInput = some pDir <* eof

pDir :: Parser Direction
pDir = do
  d <- char 'L' <|> char 'R'
  n <- L.decimal
  void (char ',' <* space) <|> eof
  case d of
    'L' -> return (L n)
    'R' -> return (R n)
    _ -> error "not right or left"
