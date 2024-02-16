{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Day12 (day12) where
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Text (Text, pack)
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void

type Parser = Parsec Void Text
data Stuff = Number Integer| Other Text
             deriving (Show, Eq)
type JParser = Parsec Void Text
data JValue = JString Text
            | JInt Integer
            | JObject [(Text, JValue)]
            | JArray [JValue]
            deriving (Show, Eq)

day12 :: IO ()
day12 = do
  -- input <- getLine
  -- parseTest pJValue  $ pack input
  input <- readFile "./inputs/Day12.txt"
  case parse pJValue "" (pack input) of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right xs -> do
      print $ "Part 1: " ++ show (objSum xs)
      print $ "Part 2: " ++ show (objSumNoRed xs)

pJValue :: JParser JValue
pJValue = choice [ JString <$> pString
                 , JArray <$> pJArray
                 , JInt <$> pJInt
                 , JObject <$> pJObject
                 ]

pString :: JParser Text
pString = between (char '\"') (char '\"') (takeWhileP Nothing (/= '\"')) <?> "String"

pJArray :: JParser [JValue]
pJArray = between (char '[') (char ']') arr <?> "Array"
  where arr = sepBy pJValue (char ',')

pJInt :: JParser Integer
pJInt = L.signed space L.decimal <?> "Integer"

pJObject :: JParser [(Text, JValue)]
pJObject = between (char '{') (char '}') obj <?> "Object"
  where obj = sepBy field (char ',')
        field = (,) <$> (pString <* char ':') <*> pJValue

objSum :: JValue -> Integer
objSum jval = case jval of
  JString _ -> 0
  JArray arr -> sum $ map objSum arr
  JInt val -> val
  JObject obj -> sum $ map (objSum . snd) obj

objSumNoRed :: JValue -> Integer
objSumNoRed jval = case jval of
  JString _ -> 0
  JArray arr -> sum $ map objSumNoRed arr
  JInt val -> val
  JObject obj -> if not $ any ((== JString "red") . snd) obj
                 then sum $ map (objSumNoRed . snd) obj
                 else 0

myTest :: Parser [Stuff]
myTest = some (Other <$> other <|> Number <$> num)

num :: Parser Integer
num = L.signed space L.decimal

other :: Parser Text
other =  takeWhile1P Nothing (\x -> x `notElem` ['0'..'9'] ++ "-")

filterStuff :: [Stuff] -> [Integer]
filterStuff stuff = map toInt (filter fil stuff)
  where
    fil s = case s of
      Number _ -> True
      _ -> False
    toInt (Number x) = x
