module Day08 (day08) where
import Text.Regex.Posix

day08 :: IO ()
day08 = do
  input <- readFile "./inputs/Day08.txt"
  let inputLines = lines input
  let part1 = sum $ map computeDifference inputLines
  let part2 = sum $ map (\x -> length (show x) - (length x)) inputLines
  print $ unwords ["Part 1: ", show part1]
  print $ unwords ["Part 2: ", show part2]


computeDifference :: String -> Int
computeDifference str =
  quotes + backslashes + 3 * chars
  where
    backslashes = str =~ "\\\\\\\\"
    quotes = str =~ "\\\"" 
    chars = str =~ "\\\\x[0-9a-f]{2}"
  -- let pat = "(\\\")|(\\\\)|(\\x[0-9a-f]{2})"

matchMake :: String -> ([[String]], [[String]], [[String]])
matchMake str = (backslashes, chars, quotes)
  where
    backslashes = str =~ "\\\\\\\\"
    chars = str =~ "(\\\\x[0-9a-f]{2})"
    quotes = str =~ "\\\""
