{-# LANGUAGE OverloadedStrings #-}
module Y2016.Day04 (day04) where
import Text.Megaparsec
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec.Char (space, char, lowerChar)
import Control.Monad (void)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.List (sortOn, group, sort)
import Data.Char (ord, chr)

type Parser = Parsec Void Text

type Room = (String, Int, String)

day04 :: IO ()
day04 = do
  input <- readFile "./inputs/2016/day04.txt"
  case parse (some pRoom) "" (pack input) of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right result -> do
      let fRooms = filter isReal result
          p1 = sum $ (\(_, n, _) -> n) <$> fRooms
          p2 = decrypt <$> fRooms
      print $ "Part 1: " ++ show p1
      putStrLn "Part 2: "
      mapM_ print fRooms 
      mapM_ print p2

isReal :: Room -> Bool
isReal (name, _, hash) = hash == hash'
  where
  hash' = take 5 (head <$> (sortOn ((* (-1)) . length) . group . sort) name)

decrypt :: Room -> Room
decrypt (name, num, hash) = (shiftN <$> name, num, hash)
  where
  shiftN :: Char -> Char
  shiftN c = chr $ ((ord c - ord 'a' + num) `mod` 26) + ord 'a'



pRoom :: Parser Room
pRoom = do
  name <- some (some lowerChar <* char '-')
  num <- L.decimal
  hash <- between (char '[') (char ']') (some lowerChar)
  void space <|> eof
  return (mconcat name, num, hash)
