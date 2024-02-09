module Day11 (day11, next, check) where
import Text.Regex.TDFA ( (=~) )


day11 :: IO ()
day11 = do
  input <- readFile "./inputs/Day11.txt"
  let passwords = iterate next input
  print $ take 5 $ filter check passwords

next :: String -> String
next (x:xs) = if all (== 'z') xs
              then succ x : map (const 'a') xs
              else x : next xs
next [] = ['a']

check :: String -> Bool
check a = conseq a && omit a && doubles a
  where
    conseq :: String -> Bool
    conseq b = b =~ "abc|bce|cde|def|efg|fgh|pqr|qrs|rst|stu|tuv|uvw|vwx|wxy|xyz"
    omit :: String -> Bool
    omit b = not $ b =~ "i|l|o"
    doubles :: String -> Bool
    doubles b = (b =~ "aa|bb|cc|dd|ee|ff|gg|hh|ii|jj|kk|ll|mm|nn|oo|pp|qq|rr|ss|tt|uu|vv|ww|xx|yy|zz" :: Int) >= 2
