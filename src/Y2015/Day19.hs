{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE InstanceSigs #-}
module Y2015.Day19 (day19) where
import Text.Megaparsec
    ( (<|>), optional, chunk, parse, errorBundlePretty, some, Parsec )
import Text.Megaparsec.Char ( char, lowerChar, space, upperChar )
import Data.Text (Text, pack)
import Data.Void (Void)
import Data.Maybe (maybeToList)
import Control.Monad (void)
import Data.List (elemIndices, nub)

type Parser = Parsec Void Text
newtype El = El String deriving (Eq, Ord)
type Molecule = [El]
type Equation = (El, Molecule)

instance Show El where
  show :: El -> String
  show (El x) = show x


day19 :: IO ()
day19 = do
  input <- readFile "./inputs/2015/day19.txt"
  case parse pInput "" (pack input) of
    Left bundle -> putStr (errorBundlePretty bundle)
    Right xs -> do
      let (eq', mole) = xs
          eq = [e | e <- eq', El "C" `notElem` snd e]
          p1 = length $ nub (replaceAll eq mole)
          p2 = countDiffs mole 
      -- listEle eq mole
      print $ "Part 1: " ++ show p1
      print $ "Part 2: " ++ show p2

-- listEle :: [Equation] -> Molecule -> IO ()
-- listEle eq m = do
--   putStrLn $ "Left: " ++ show l
--   putStrLn $ "Right: " ++ show r
--   putStrLn $ "Exclude: " ++ show exclusions
--   putStrLn $ "Only Products: " ++ show onlyProducts 
--   putStrLn $ "Prefixes: " ++ show pref
--   putStrLn $ "Suffixes: " ++ show suf
--   putStrLn $ "OnlyPrefix: " ++ show onlyPref
--   putStrLn $ "OnlySuffix: " ++ show onlySuf
--   putStrLn $ "Large: " ++ show (length large) ++ show large
--   putStrLn $ "Very Large: " ++ show (length large2) ++ show large2
--     where
--     l = sort . nub $ map fst eq
--     r = sort . nub . concat $ map snd eq
--     onlyProducts = [ele | ele <- r, ele `notElem` l]
--     exclusions = [ele |ele <- onlyProducts, ele `notElem` m]
--     pref =  [ele | ele <- r, any (\x -> [ele] `isPrefixOf` snd x) eq]
--     suf = [ele | ele <- r, any (\x -> [ele] `isSuffixOf` snd x) eq]
--     onlyPref = [ele | ele <- pref, all (\x -> ele `notElem` (tail . snd) x) eq]
--     onlySuf = [ele | ele <- suf, all (\x -> ele `notElem` (init . snd) x) eq]
--     large = [e | e <- eq, (length . snd) e > 2]
--     large2 = [e | e <- eq, (length . snd) e > 4]

--None of my attempts at creating an efficient algorithm were successful,
--but we can exploit that most equations add 1 to the number of elements.
--All other equations end in Ar, and they all have either 4 or 6 products.
--The ones with 6 products all have the pattern FYFAr, and the Y is unique to
--These reactions. The simple formula for number of substitutions is:
--(Length - 1) - (2 * Ar) - (2 * Y)

countDiffs :: Molecule -> Int
countDiffs m = length m - 1 - 2*ars - 2*ys
  where
  ars = length $ elemIndices (El "Ar") m
  ys = length $ elemIndices (El "Y") m

-- countDiffs :: [Equation] -> Molecule -> Molecule -> Maybe Int
-- countDiffs _ [] [] = Just 0
-- countDiffs _ (_:_) [] = Nothing
-- countDiffs _ [] (_:_) = Nothing
-- countDiffs eqs end (y:ys)
--   | end == (y:ys) = Just 0
--   | null ys && any (\eq -> y == fst eq && end == snd eq) eqs = Just 1
--   | isJust splt = minMaybe diffs
--   | otherwise = trace (show end ++ show (y:ys)) (error "see trace")
--     where
--     diffs = do
--       eq <- filter ((elSplt `elem`) . snd) eqs
--       let preEq = takeWhile (/= elSplt) (snd eq)
--           postEq = dropWhile (/= elSplt) (snd eq)
--       return (countDiffs eqs s1)
--     splt = fromMaybe (-1) $ findIndex (`elem` immutables) end
--     elSplt = end !! splt
--     (s1, s2) = splitAt (splt + 1) end
--     immutables = [ele | ele <- r, ele `notElem` l]
--     l = sort . nub $ map fst eqs
--     r = sort . nub . concat $ map snd eqs


-- minMaybe :: [Maybe Int] -> Maybe Int
-- minMaybe [] = Nothing
-- minMaybe (x:xs)
--   | isNothing x = minMaybe xs
--   | otherwise = min' x (minMaybe xs)
--     where
--     min' Nothing (Just s) = Just s
--     min' (Just s) Nothing = Just s
--     min' (Just s) (Just t) = Just $ min s t
--     min' Nothing Nothing = Nothing

-- countDiffs :: [Equation] -> Molecule -> Molecule -> Maybe Int
-- countDiffs _ [] [] = Just 0
-- countDiffs _ (x:xs) [] = Nothing
-- countDiffs _ [] (y:ys) = Nothing
-- countDiffs eqs (x:xs) (y:ys)
--   | (x:xs) == (y:ys) = Just 0
--   -- | x == y = countDiffs eqs xs ys
--   | otherwise = minMaybe diffs
--     where
--     diffs = do
--       (s1, s2) <- map (\n -> splitAt n (x:xs)) [1..length (x:xs) - 1]
--       eq <- filter ((==y) . fst) eqs
--       if length (y:ys) == 1 then pure $ (+1) <$> countDiffs eqs (x:xs) (snd eq)
--       else pure $ sum <$> sequence [countDiffs eqs s1 [y],
--                                     countDiffs eqs s2 ys]



replaceAllEq :: Equation -> Molecule -> [Molecule]
replaceAllEq e m = map (\i -> sub i m) (elemIndices (fst e) m)
  where
  sub ind _ = hd ++ snd e ++ rest
    where (hd, _:rest) = splitAt ind m

replaceAll :: [Equation] -> Molecule -> [Molecule]
replaceAll e m = (`replaceAllEq` m) =<< e

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



