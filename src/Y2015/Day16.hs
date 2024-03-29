{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Y2015.Day16 (day16) where
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void (Void)
import Data.List (elemIndices)
import Control.Monad (guard)


type Parser = Parsec Void Text
data Thing a  = Children a
              | Cats a
              | Samoyeds a
              | Pomeranians a
              | Akitas a
              | Vizslas a
              | Goldfish a
              | Trees a
              | Cars a
              | Perfumes a
              deriving (Eq, Show)
type SueThing = Thing Int
type Sue = [SueThing]

mySue :: Sue
mySue = [Children 3, Cats 7, Samoyeds 2, Pomeranians 3, Akitas 0,
         Vizslas 0, Goldfish 5, Trees 3, Cars 2, Perfumes 1]


day16 :: IO ()
day16 = do
  -- input <- getLine
  input <- readFile "./inputs/2015/day16.txt"
  case parse (some pSue) "" (pack input) of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right sues -> do
      let p1 = head (matchSue sues mySue) + 1
      let p2 = head (matchSue2 sues mySue) + 1
      print $ "Part 1: " ++ show p1
      print $ "Part 2: " ++ show p2

matchSue :: [Sue] -> Sue -> [Int]
matchSue sueList sue = elemIndices (head sueMatch) sueList
  where sueMatch = do
          s <- sueList
          guard (all (`elem` sue) s)
          return s

matchSue2 :: [Sue] -> Sue -> [Int]
matchSue2 sueList sue = elemIndices (head sueMatch) sueList
  where
    sueMatch = do
      s <- sueList
      guard (all (`isSat` sue) s)
      return s
    isSat :: SueThing -> Sue -> Bool
    isSat sueThing sue' = case sueThing of
      Cats n -> n > 3
      Trees n -> n > 3
      Pomeranians n -> n < 3
      Goldfish n -> n < 5
      _ -> sueThing `elem` sue'

pSue :: Parser Sue
pSue = do
  _ <- space <* chunk "Sue " <* L.decimal <* chunk ": "
  t1 <- pSueThing <* chunk ", "
  t2 <- pSueThing <* chunk ", "
  t3 <- pSueThing
  return [t1, t2, t3]

pSueThing :: Parser SueThing
pSueThing = do
  thing <- some alphaNumChar <* chunk ": "
  num <- L.decimal
  return (case thing of
    "children" -> Children num
    "cats" -> Cats num
    "samoyeds" -> Samoyeds num
    "pomeranians" -> Pomeranians num
    "akitas" -> Akitas num
    "vizslas" -> Vizslas num
    "goldfish" -> Goldfish num
    "trees" -> Trees num
    "cars" -> Cars num
    "perfumes" -> Perfumes num
    _ -> error "Invalid")


