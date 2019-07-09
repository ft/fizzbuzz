module Main where
import System.Environment

fizzbuzz :: Integer -> String
fizzbuzz n =      if n `mod` 3 == 0 && n `mod` 5 == 0 then "fizzbuzz"
             else if n `mod` 3 == 0                   then "fizz"
             else if n `mod` 5 == 0                   then "buzz"
             else show n

main :: IO ()
main = getArgs >>= parse >>= runFizzBuzz
  where parse [n] = return $ read n
        parse  _  = return 15
        runFizzBuzz n = putStr $ unlines $ map fizzbuzz [1..n]
