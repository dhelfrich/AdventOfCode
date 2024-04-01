
module Y2016.Day05 (day05) where
import Crypto.Hash
import Data.ByteString.Char8 (pack)
import qualified Data.Map as M
import Data.Char (ord)

day05 :: IO ()
day05 = do
  input <- readFile "./inputs/2016/day05.txt"
  let hashMap = map (\x -> (x, show (hashNum input x))) [1..]
      first5 = take 8 $  filter (\x -> take 5 (snd x) == "00000") hashMap
      p1 = (!! 5) . snd <$> first5
      map2 = mapP2 input 0 M.empty
      p2 = (map snd . M.toList) map2
  print $ "Part 1: " ++ p1
  print $ "Part 2: " ++ p2

hashNum :: String -> Int -> Digest MD5
hashNum s n = hash bs
  where
  bs = pack $ s ++ show n

mapP2 :: String -> Int -> M.Map Int Char -> M.Map Int Char
mapP2 s n m
  | all (`M.member` m) [0..7] = m
  | otherwise =
      if validHash && validPos then
      mapP2 s (n+1) newMap
      else mapP2 s (n+1) m
  where
  h = hashNum s n
  validHash = (take 5 . show) h == "00000"
  validPos = (show h !! 5) `elem` ['0'..'7']
  pos = ord (show h !! 5) - ord '0'
  c = show h !! 6
  insFun _ old = old
  newMap = M.insertWith insFun pos c m