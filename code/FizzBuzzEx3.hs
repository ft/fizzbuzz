module Main where
import System.Environment

(⊘) :: Integral a => a -> a -> Bool
(⊘) n m = n `mod` m == 0

(★) :: String -> String -> String
"" ★ s = s
aa ★ _ = aa

fizzbuzz :: Integer -> String
fizzbuzz n = ((if n ⊘ 3 then "fizz" else "" )
           ++ (if n ⊘ 5 then "buzz" else "" ))
           ★ show n

main :: IO ()
main = getArgs >>= parse >>= runFizzBuzz
  where parse [n] = return $ read n
        parse  _  = return 15
        runFizzBuzz n = putStr $ unlines $ map fizzbuzz [1..n]
