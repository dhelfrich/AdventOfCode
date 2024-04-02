{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Y2016.Day09 (day09) where
import Text.Megaparsec
import Data.Text (Text, pack, unpack)
import Data.Void (Void)
import Text.Megaparsec.Char (char, upperChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Debug.Trace (trace)

type Parser = Parsec Void Text
type Marker = (Int, Int)

day09 :: IO ()
day09 = do
  input <- readFile "./inputs/2016/day09.txt"
  case parse (some pInput) "" (pack input) of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right result -> do
      let p1 = getLength result
          p2 = getLength2 result
      print $ "Part 1: " ++ show p1
      print $ "Part 2: " ++ show p2

getLength :: [(Marker, String)] -> Int
getLength li = sum $ map (\((_, n), s) -> length s * n) li

getLength2 :: [(Marker, String)] -> Int
getLength2 xs = sum $ map getLength2' xs
  where
  getLength2' ((_, n), s)
    | 'x' `notElem` s = length s * n
    | otherwise = getLength2 (pStr s) * n

pStr :: String ->  [(Marker, String)]
pStr s = case parse (some pInput) "" (pack s) of
    Left bundle -> trace (errorBundlePretty bundle) []
    Right result -> result

pInput :: Parser (Marker, String)
pInput = pMarkedStr <|> pUnmarkedStr

pMarkedStr :: Parser (Marker, String)
pMarkedStr = do
  (l, n) <- pMarker
  s <- takeP (Just "repeated") l
  return ((l, n), unpack s)

pUnmarkedStr :: Parser (Marker, String)
pUnmarkedStr = do
  s <- some upperChar
  return ((length s, 1), s)

pMarker :: Parser Marker
pMarker = between (char '(') (char ')')
  ((,) <$> (L.decimal <* char 'x') <*> L.decimal)

