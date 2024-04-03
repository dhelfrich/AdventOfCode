module Main (main) where
import System.IO (stdout)
import System.Console.ANSI

import Y2016.Day11



main :: IO ()
main = do
  stdoutSupportsANSI <- hNowSupportsANSI stdout
  if stdoutSupportsANSI
    then do
      setSGR [SetColor Foreground Dull Blue]
      day11
      setSGR [Reset]
    else
      putStrLn "Standard output does not support 'ANSI' escape codes."