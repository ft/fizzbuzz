module Main where
import System.Environment

(⊘) :: Integral a => a -> a -> Bool
(⊘) n m = n `mod` m == 0

fizzbuzz :: (Integral a, Show a) => a -> String
fizzbuzz n = (fizz . buzz) id (show n)
  where fizz | n ⊘ 3 = \x -> const ("fizz" ++ x "")
             | otherwise = id
        buzz | n ⊘ 5 = \x -> const ("buzz" ++ x "")
             | otherwise = id

main :: IO ()
main = getArgs >>= parse >>= runFizzBuzz
  where parse [n] = return $ read n
        parse  _  = return 15
        runFizzBuzz n = putStr $ unlines $ map fizzbuzz [1..n]
