{-# LANGUAGE OverloadedStrings #-}
module Y2016.Day10 (day10) where
import Text.Megaparsec
import Data.Text (Text, pack)
import Data.Void (Void)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Map as M
import Data.List (sort, foldl')
import Debug.Trace (trace)
import Data.Maybe (mapMaybe)

type Parser = Parsec Void Text
newtype Chip = Chip Int deriving (Eq, Ord, Show)
data Loc = Bot Int | Output Int | End deriving (Show, Eq, Ord)
type Logic = M.Map Loc (Loc, Loc, [Chip])

data Line = L0 (Loc, (Loc, Loc, [Chip])) | L1 (Chip, Loc)
  deriving (Eq, Ord, Show)


day10 :: IO ()
day10 = do
  input <- readFile "./inputs/2016/day10.txt"
  case parse pInput "" (pack input) of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right result -> do
      let m = toMap result
          c = chipList result
          solved = foldl addLowestChip m c
      print c
      mapM_ print (M.toList $ (filterLogic 17 .filterLogic 61) (solved))

--Works because, when sorted, it always does the L1's last.
toMap :: [Line] -> Logic
toMap = foldl' f M.empty . sort
  where
  f m l = case l of
    L0 (loc1, (low, high, _)) -> M.insert loc1 (low, high, []) m
    L1 (_, _) -> m

chipList :: [Line] -> [(Chip, Loc)]
chipList l = sort $ mapMaybe getL1 l
  where getL1 x = case x of
          L1 (chip, loc) -> Just (chip, loc)
          L0 _ -> Nothing


addLowestChip :: Logic -> (Chip, Loc) -> Logic
addLowestChip oldLogic (chip, Output o) =
  M.insert (Output o) (End, End, [chip]) oldLogic
addLowestChip oldLogic (chip, Bot b) =
  M.insertWith combine (Bot b) (End, End, [chip]) newLogic
  where
  combine (_, _, newChip) (low, high, oldChips) = (low, high, newChip ++ oldChips)
  isLowest newChip' oldChip' = null oldChip' || newChip' < minimum oldChip'
  (low', high', oldChips') = oldLogic M.! Bot b
  newLogic =
    if isLowest chip oldChips' 
    then addLowestChip oldLogic (chip, low') 
    else addLowestChip oldLogic (chip, high')
addLowestChip _ _ = error "Error"

filterLogic :: Int -> Logic -> Logic
filterLogic n = M.filter (\(_, _, l) -> Chip n `elem` l) 

pInput :: Parser [Line]
pInput = do
  logic <- some $ (L0 <$> pLogic <|> L1 <$> pInit) <* space
  return logic

pLogic :: Parser (Loc, (Loc, Loc, [Chip]))
pLogic = do
  start <- chunk "bot " *> L.decimal <* chunk " gives low to "
  low <- pOutput <* chunk " and high to "
  high <- pOutput
  return (Bot start, (low, high, []))

pInit :: Parser (Chip, Loc)
pInit = do
  value <- chunk "value " *> L.decimal <* chunk " goes to "
  dest <- pOutput
  return (Chip value, dest)

pOutput :: Parser Loc
pOutput = (chunk "bot " *> L.decimal >>= (return . Bot)) <|>
        (chunk "output " *> L.decimal >>= (return. Output))