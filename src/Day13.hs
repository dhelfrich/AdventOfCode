{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Day13 (day13, pHappy, Happy) where
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.List (permutations, nub)
import qualified Data.Map as Map

type Parser = Parsec Void Text
data Happy = Happy {
  name1 :: String,
  name2 :: String,
  hap :: Int} deriving Show

day13 :: IO ()
day13 = do
  -- input <- getLine
  input <- readFile "./inputs/day13.txt"
  case parse (some pHappy) "" (pack input) of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right xs -> do
      print $ "Part 1: " ++ show (optimal xs)
      print $ "Part 2: " ++ show (optimal $ addSelf xs)

optimal :: [Happy] -> Int
optimal h = maximum $ calcHappy <$> permutations names
  where
    calcHappy :: [String] -> Int
    calcHappy names' = sum $ map (happyMap Map.!) (pairs names' ++ (rev <$> pairs names'))
    happyMap = Map.fromList $ map (\x -> ((name1 x, name2 x), hap x)) h
    pairs names' = zip names' (drop 1 $ take (length names' + 1) (cycle names'))
    names = nub $ map name1 h
    rev (a, b) = (b, a)

addSelf :: [Happy] -> [Happy]
addSelf h = h ++ selfPair1 ++ selfPair2
    where names = nub $ map name1 h
          selfPair1 = map (\n -> Happy {name1 = n, name2 = "self", hap = 0}) names
          selfPair2 = map (\n -> Happy {name1 = "self", name2 = n, hap = 0}) names

pHappy :: Parser Happy
pHappy = do
  _ <- space
  name1' <- some alphaNumChar
  _ <- chunk " would "
  -- sign <- chunk "gain " <|> chunk "lose "
  num <- (* 1) <$> (chunk "gain " *> L.decimal) <|>
         (*(-1)) <$> (chunk "lose " *> L.decimal)
  _ <- chunk " happiness units by sitting next to "
  name2' <- some alphaNumChar
  _ <- char '.'
  return Happy {name1 = name1', name2 = name2', hap = num}