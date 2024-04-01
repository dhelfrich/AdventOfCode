{-# LANGUAGE OverloadedStrings #-}
module Y2016.Day07 where
import Text.Megaparsec
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec.Char (space, char, lowerChar)
import Data.List (tails)

type Parser = Parsec Void Text

day07 :: IO ()
day07 = do
  input <- readFile "./inputs/2016/day07.txt"
  case parse (some pLine) "" (pack input) of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right result -> do
      let p1 = length $ (any isTLS . fst . toTuple) `filter` result
      let p2 = length $ isSSL `filter` result
      print $ "Part 1: " ++ show p1
      print $ "Part 2: " ++ show p2

isTLS :: String -> Bool
isTLS x = any abba (fours x)
  where
  fours s = take (length s - 4) (take 4 <$> tails s)
  abba [x1, x2, x3, x4] = (x1 == x4) && (x2 == x3) && (x1 /= x2)
  abba _ = False

isSSL :: [String] -> Bool
isSSL str = or [comp outs ins |
                  outs <- outside >>= threes, isABA outs,
                  ins <- inside >>= threes, isABA ins]
  where
  comp [a1, a2, _] [b1, b2, _] = a1 == b2 && a2 == b1
  comp _ _ = False
  isABA [x1, x2, x3] = (x1 == x3) && (x1 /= x2)
  isABA _ = False
  threes s = filter ((==3) . length) $ take 3 <$> tails s
  (outside, inside) = toTuple str

toTuple :: [String] -> ([String], [String])
toTuple [] = ([], [])
toTuple [x] = ([x], [])
toTuple (x1:x2:xs) = (x1 : x1s, x2 : x2s)
  where (x1s, x2s) = toTuple xs

pLine :: Parser [String]
pLine = some (choice [
    some lowerChar,
    between (char '[') (char ']') (some lowerChar)]) <* (space <|> eof)