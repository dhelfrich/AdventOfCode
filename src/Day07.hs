{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day07 (day07) where
import Text.Regex.TDFA ((=~))
import Data.Word (Word16)
import Data.Char (isDigit)
import qualified Data.Map as M
import qualified Data.Bits as B

type Circuit = M.Map String Word16
data ComputationType = Number | And | Or | Not | LShift | RShift deriving (Eq,Show)
data Computation = Computation {
    computationType :: ComputationType,
    inputs :: [String]
    }
  deriving (Eq, Show)

day07 :: IO ()
day07 = do
  input <- readFile "./inputs/Day07.txt"
  let instructions = map parseInstruction $ lines input
  let start = M.fromList $ zip (map (last . inputs) instructions) ([0, 0..] :: [Word16])
  let execute circuit = foldl executeInstruction circuit instructions
  let part1 = iterate execute start !! 400
  let execute2 circuit = foldl executeInstruction2 circuit instructions
  let part2 = iterate execute2 start !! 400
  let part1Value = part1 M.! "a"
  let part2Value = part2 M.! "a"
  print $ "Part 1: " ++ show part1Value
  print $ "Part 2: " ++ show part2Value

parseInstruction :: String -> Computation
parseInstruction line =
  case instructionType of
    "AND" -> Computation And [inp1, inp2, dest]
    "OR" -> Computation Or [inp1, inp2, dest]
    "NOT" -> Computation Not [inp1', dest']
    "LSHIFT" -> Computation LShift [inp1, inp2, dest]
    "RSHIFT" -> Computation RShift [inp1, inp2, dest]
    _ -> Computation Number [inp1', dest']
  where
  instructionType = line =~ "(AND|OR|NOT|LSHIFT|RSHIFT)"
  matches = line =~ "[a-z0-9]+"
  [[inp1], [inp2], [dest]] = matches
  [[inp1'], [dest']] = matches

executeInstruction :: Circuit -> Computation -> Circuit
executeInstruction circuit computation =
  M.insert dest value circuit
  where
  value = case computationType computation of
    Number -> convert inp1'
    Not -> B.complement $ convert inp1'
    And -> convert inp1 B..&. convert inp2
    Or -> convert inp1 B..|. convert inp2
    LShift -> B.shiftL (convert inp1) $ fromEnum $ convert inp2
    RShift -> B.shiftR (convert inp1) $ fromEnum $ convert inp2
  convert :: String -> Word16
  convert inp = if isDigit $ head inp then read inp else circuit M.! inp
  [inp1, inp2, _] = inputs computation
  [inp1', _] = inputs computation
  dest = last $ inputs computation

executeInstruction2 :: Circuit -> Computation -> Circuit
executeInstruction2 circuit computation =
  if dest == "b" then M.insert dest 3176 circuit else
  M.insert dest value circuit
  where
  value = case computationType computation of
    Number -> convert inp1'
    Not -> B.complement $ convert inp1'
    And -> convert inp1 B..&. convert inp2
    Or -> convert inp1 B..|. convert inp2
    LShift -> B.shiftL (convert inp1) $ fromEnum $ convert inp2
    RShift -> B.shiftR (convert inp1) $ fromEnum $ convert inp2
  convert :: String -> Word16
  convert inp = if isDigit $ head inp then read inp else circuit M.! inp
  [inp1, inp2, _] = inputs computation
  [inp1', _] = inputs computation
  dest = last $ inputs computation



