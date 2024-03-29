{-# LANGUAGE OverloadedStrings #-}
module Y2016.Day04 (day04) where
import Text.Megaparsec
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec.Char (space, char)
import Control.Monad (void)
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

type Room = (String, Int, String)

day04 :: IO ()
day04 = do
  input <- readFile "./inputs/2016/day04.txt"
  print "hi"
  -- case parse pInput "" (pack input) of
  --   Left bundle -> putStr (errorBundlePretty bundle)
  --   Right result -> do
  --     print "hi"

-- pRoom :: Parser Room