module Fib (runFib) where

runFib :: IO ()
runFib = do
  n <- readLn
  print (fib n)

fib :: Integer -> Integer
fib n 
  | n < 2 = n
  | otherwise = fib (n - 1) + fib (n - 2)
