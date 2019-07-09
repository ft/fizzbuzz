module Main where
import System.Environment

fizzbuzz :: (Integral a, Show a) => a -> String
fizzbuzz n = (test 3 "fizz" . test 5 "buzz") id (show n)
  where test d s x | n `mod` d == 0 = const (s ++ x "")
                   | otherwise = x

main :: IO ()
main = getArgs >>= parse >>= runFizzBuzz
  where parse [n] = return $ read n
        parse  _  = return 15
        runFizzBuzz n = putStr $ unlines $ map fizzbuzz [1..n]
