{-# LANGUAGE OverloadedStrings #-}
module Y2016.Day08 where
import Text.Megaparsec
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec.Char (space, char, alphaNumChar)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void, liftM2, forM_)
import qualified Data.Map as M
import Data.Foldable (for_)
import Data.Traversable (for)

type Parser = Parsec Void Text
data Instruction = Rect Int Int
                 | Rrow Int Int
                 | Rcolumn Int Int deriving (Show)
data Grid = Grid {isOn :: (M.Map (Int, Int) Bool)
                 ,gridW :: Int
                 ,gridH :: Int} deriving (Show)

day08 :: IO ()
day08 = do
  input <- readFile "./inputs/2016/day08.txt"
  case parse (some pInst) "" (pack input) of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right result -> do
      let grid = foldl doIns (startGrid 50 6) result
      -- print $ scanl doIns (startGrid 7 3) result
      print $ "Part 1: " ++ show (countTrue $ isOn grid)
      printGrid grid


doIns :: Grid -> Instruction -> Grid
doIns g i = g {isOn = mapFun (isOn g)}
  where
  mapFun = case i of
    Rect w h -> M.mapWithKey (\ (x, y) v -> if x < w && y < h then True else v)
    Rrow row n -> M.mapKeys (\ (x, y) ->
                  if y == row then ((x + n) `mod` gridW g, y) else (x,y))
    Rcolumn col n -> M.mapKeys (\ (x, y) ->
                  if x == col then (x, (y + n) `mod` gridH g) else (x,y))

printGrid :: Grid -> IO ()
printGrid g = do 
  forM_ [0.. gridH g - 1] (\y -> putStrLn 
    [toPic $ isOn g M.! (x, y) | x <- [0..(gridW g - 1)] ])
  where
  toPic b = if b then '#' else '.'

startGrid :: Int -> Int -> Grid
startGrid x y = Grid
  {isOn = M.fromList $ zip (liftM2 (,) [0..x-1] [0..y-1]) (repeat False)
  ,gridW = x, gridH = y}

countTrue :: M.Map (Int, Int) Bool -> Integer
countTrue = M.foldl ff 0
  where ff n b = if b then n + 1 else n

pInst :: Parser Instruction
pInst = (pRect <|> pShift) <* (space <|> eof)

pRect :: Parser Instruction
pRect = Rect <$> (chunk "rect " *> L.decimal <* char 'x') <*> L.decimal

pShift :: Parser Instruction
pShift = do
  void $ chunk "rotate "
  dir <- (chunk "row" <|> chunk "column") <* space <* alphaNumChar <* char '='
  rc <- L.decimal <* chunk " by "
  n <- L.decimal
  return (case dir of
    "row" -> Rrow rc n
    "column" -> Rcolumn rc n
    _ -> error "not row or column")
