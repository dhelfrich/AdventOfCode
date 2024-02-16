{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
module Day19 where
import Text.Megaparsec
    ( (<|>), optional, chunk, parse, errorBundlePretty, some, Parsec )
import Text.Megaparsec.Char ( char, lowerChar, space, upperChar )
import Data.Text (Text, pack)
import Data.Void (Void)
import Data.Maybe (maybeToList, fromMaybe, isNothing)
import Control.Monad (void)
import Data.List (elemIndices, nub, findIndex, iterate', findIndices, isPrefixOf, tails, sort, isSuffixOf)
import GHC.Utils.Error (errorDiagnostic, errorMsg)

type Parser = Parsec Void Text
newtype El = El String deriving (Eq, Ord)
type Molecule = [El]
type Equation = (El, Molecule)

instance Show El where
  show (El x) = show x

toStr :: El -> String
toStr (El s) = s

toStr' :: [El] -> String
toStr' es = mconcat $ map toStr es

day19 :: IO ()
day19 = do
  input <- readFile "./inputs/day19.txt"
  case parse pInput "" (pack input) of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right xs -> do
      let (eq, mole) = xs
          p1 = length $ nub (replaceAll eq mole)
          p2 = countDiffs eq mole [El "e"]
          t1 = iterate (nub . replaceAll' eq) [[El "e"]]
          t1' = take 5 (t1!!2)
          -- p2 = length mole
      listEle eq mole
      print $ t1'
      print "\n"
      -- print eq
      -- print mole
      print $ replaceAll eq [El "e"]
      print $ "Part 1: " ++ show p1
      -- print $ map (\t -> countDiffs eq t [El "e"]) t1'
      -- print $ "Part 2: " ++ show p2

listEle :: [Equation] -> Molecule -> IO ()
listEle eq m = do
  putStrLn $ "Left:" ++ show l
  putStrLn $ "Right:" ++ show r
  putStrLn $ "Only Products: " ++ show [ele | ele <- r, ele `notElem` l]
  putStrLn $ "Prefixes" ++ show [ele | ele <- r, any (\x -> [ele] `isPrefixOf` snd x) eq]
  putStrLn $ "Suffixes" ++ show [ele | ele <- r, any (\x -> [ele] `isSuffixOf` snd x) eq]
    where
    l = sort . nub $ map fst eq
    r = sort . nub . concat $ map snd eq


countDiffs :: [Equation] -> Molecule -> Molecule -> Maybe Int
countDiffs _ [] [] = Just 0
countDiffs _ (x:xs) [] = Nothing
countDiffs _ [] (y:ys) = Nothing
countDiffs eqs (x:xs) (y:ys)
  | (x:xs) == (y:ys) = Just 0
  -- | x == y = countDiffs eqs xs ys
  | otherwise = minMaybe diffs
    where
    diffs = do
      (s1, s2) <- map (\n -> splitAt n (x:xs)) [1..length (x:xs) - 1]
      eq <- filter ((==y) . fst) eqs
      if length (y:ys) == 1 then pure $ (+1) <$> countDiffs eqs (x:xs) (snd eq)
      else pure $ sum <$> sequence [countDiffs eqs s1 [y],
                                    countDiffs eqs s2 ys]

minMaybe :: [Maybe Int] -> Maybe Int
minMaybe [] = Nothing
minMaybe (x:xs)
  | isNothing x = minMaybe xs
  | otherwise = min' x (minMaybe xs)
    where 
    min' Nothing (Just s) = Just s
    min' (Just s) Nothing = Just s
    min' (Just s) (Just t) = Just $ min s t
    min' Nothing Nothing = Nothing



-- countDiffs :: [Equation] -> Molecule -> Molecule -> Int
-- countDiffs _ [] [] = 0
-- countDiffs _ _ [] = 99996
-- countDiffs _ [] (y:ys) = 99997
-- countDiffs eqs (x:xs) (y:ys)
--   | (x:xs) == (y:ys) = 0
--   | length (y:ys) > length (x:xs) = 99998
--   | all ((/= x) . head . snd) poss = 99999
--   | otherwise = do
--   let sub eq = 1 + (countDiffs eqs (x:xs) (snd eq ++ ys))
--       subs = map sub poss
--       sub2 = [countDiffs eqs xs ys | x == y]
--    in sum (subs ++ sub2 ++ [99995])
--     where
--     poss = filter ((== y) . fst) eqs


replaceAllEq :: Equation -> Molecule -> [Molecule]
replaceAllEq e m = map (\i -> sub i m) (elemIndices (fst e) m)
  where
  sub ind _ = hd ++ snd e ++ rest
    where (hd, _:rest) = splitAt ind m

replaceAll :: [Equation] -> Molecule -> [Molecule]
replaceAll e m = (`replaceAllEq` m) =<< e

replaceAll' :: [Equation] -> [Molecule] -> [Molecule]
replaceAll' e m = do
  mole <- m
  eq <- e
  replaceAllEq eq mole

countCycles :: [Equation] -> Molecule -> Int
countCycles e m = fromMaybe (-1) (findIndex (m `elem`) iterations)
  where
  iterations :: [[Molecule]]
  iterations = iterate' (nub . replaceAll' e) [[El "e"]]

-- countCyclesRev :: [Equation] -> Molecule -> Int
-- countCyclesRev e m = fromMaybe (-1) (findIndex (elem [El "e"]) iterations)
--   where
--   iterations = iterate' (nub . (replaceAllRev e)) [m]

-- replaceAllRev :: [Equation] -> [Molecule] -> [Molecule]
-- replaceAllRev e m = do
--   eq <- e
--   mole <- m
--   index <- findIndices (isPrefixOf $ snd eq) (tails mole)
--   let (firstPart, secondPart) = splitAt index mole
--     in [firstPart ++ [fst eq] ++ drop (length $ snd eq) secondPart]



pInput :: Parser ([Equation], Molecule)
pInput = do
  eqs <- some (pEquation <* char '\n')
  void space
  mole <- some pEl
  return (eqs, mole)

pEl :: Parser El
pEl = do
  a <- upperChar <|> char 'e'
  b <- optional lowerChar
  return (El  (a : maybeToList b))

pEquation :: Parser Equation
pEquation = do
  ele <- pEl
  mole <- chunk " => " *> some pEl
  return (ele, mole)



