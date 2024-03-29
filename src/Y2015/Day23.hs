{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
module Y2015.Day23 (day23) where
import Text.Megaparsec (empty, parse, choice, Parsec, (<|>), errorBundlePretty, some)
import Text.Megaparsec.Char ( space, string, char )
import Data.Text (Text, pack)
import Data.Void (Void)
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad (void)

type Parser = Parsec Void Text
data Register = A | B deriving (Show)
data Instruction = Hlf Register
                 | Tpl Register
                 | Inc Register
                 | Jmp Int
                 | Jie Register Int
                 | Jio Register Int
                 deriving (Show)
type Pos = Int

data State = State Pos Int Int
  deriving (Show)

day23 :: IO ()
day23 = do
  input <- readFile "./inputs/2015/day23.txt"
  case parse pInput "" (pack input) of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right xs -> do
      print $ "Part 1: " ++ show p1
      print $ "Part 2: " ++ show p2
        where State _ _ p1 = exec xs (State 0 0 0)
              State _ _ p2 = exec xs (State 0 1 0)

exec :: [Instruction] -> State -> State
exec ins (State pos aReg bReg)
  | pos > length ins - 1 = State pos aReg bReg
  | otherwise = exec ins newState
    where
      newState = case ins !! pos of
        Hlf A -> State (pos + 1) (aReg `div` 2) bReg
        Hlf B -> State (pos + 1) aReg (bReg `div` 2)
        Tpl A -> State (pos + 1) (aReg * 3) bReg
        Tpl B -> State (pos + 1) aReg (bReg * 3)
        Inc A -> State (pos + 1) (aReg + 1) bReg
        Inc B -> State (pos + 1) aReg (bReg + 1)
        Jmp x -> State (pos + x) aReg bReg
        Jie A x -> case even aReg of
          True -> State (pos + x) aReg bReg
          False -> State (pos + 1) aReg bReg
        Jie B x -> case even bReg of
          True -> State (pos + x) aReg bReg
          False -> State (pos + 1) aReg bReg
        Jio A x -> case aReg == 1 of
          True -> State (pos + x) aReg bReg
          False -> State (pos + 1) aReg bReg
        Jio B x -> case bReg == 1 of
          True -> State (pos + x) aReg bReg
          False -> State (pos + 1) aReg bReg


-- initState :: State
-- initState = State 0 0 0

pInput :: Parser [Instruction]
pInput = some (pInstruction <* space)

pFunc :: Parser Instruction
pFunc = choice
  [Hlf <$ string "hlf" <*> pRegister,
   Tpl <$ string "tpl" <*> pRegister,
   Inc <$ string "inc" <*> pRegister
  ]

pJump :: Parser Instruction
pJump = choice
  [Jmp <$ string "jmp " <*> L.signed sc L.decimal,
   Jie <$ string "jie" <*> pRegister <* string ", " <*> L.signed sc L.decimal,
   Jio <$ string "jio" <*> pRegister <* string ", " <*> L.signed sc L.decimal ]


pRegister :: Parser Register
pRegister = space *> ((A <$ char 'a') <|> (B <$ char 'b'))

pInstruction :: Parser Instruction
pInstruction = pFunc <|> pJump

sc :: Parser ()
sc = L.space (void $ some (char ' ' <|> char '\t')) empty empty