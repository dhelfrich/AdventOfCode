module Day05 (day05) where
import Text.Regex.Posix

day05 :: IO ()
day05 = do
    input <- readFile "./inputs/Day05.txt"
    let answer1 = show . length $ filter testString1 $ lines input 
    let answer2 = show . length $ filter testString2 $ lines input 
    print $ "Part 1: " ++ answer1
    print $ "Part 2: " ++ answer2

testString1 :: String -> Bool
testString1 str = test1 str && test2 str && test3 str

testString2 :: String -> Bool
testString2 str = test4 str && test5 str

test1 :: String -> Bool
test1 str = (str =~ "[aeiou]" :: Int) >= 3

test2 :: String -> Bool
test2 str = str =~ "(.)\\1" :: Bool

test3 :: String -> Bool
test3 str = not (str =~ "(ab)|(cd)|(pq)|(xy)" :: Bool)

test4 :: String -> Bool
test4 str = str =~ "(.{2}).*\\1" :: Bool

test5 :: String -> Bool
test5 str = str =~ "(.).\\1" :: Bool